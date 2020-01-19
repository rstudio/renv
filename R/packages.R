
renv_package_installed <- function(package, lib.loc = NULL) {
  location <- find.package(package, lib.loc = lib.loc, quiet = TRUE)
  length(location) > 0
}

renv_package_version <- function(package) {
  renv_package_description_field(package, "Version")
}

renv_package_description_field <- function(package, field) {
  path <- renv_package_find(package)
  desc <- renv_description_read(path)
  desc[[field]]
}

renv_package_type <- function(path, quiet = FALSE, default = "source") {

  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("no package at path '%s'", aliased_path(path))

  # for directories, check for Meta
  if (info$isdir) {
    hasmeta <- file.exists(file.path(path, "Meta"))
    type <- if (hasmeta) "binary" else "source"
    return(type)
  }

  # otherwise, guess based on contents of package
  methods <- list(
    tar = function(path) untar(tarfile = path, list = TRUE),
    zip = function(path) unzip(zipfile = path, list = TRUE)$Name
  )

  # guess appropriate method when possible
  type <- renv_archive_type(path)
  if (type %in% c("tar", "zip"))
    methods <- methods[type]

  for (method in methods) {

    files <- catch(method(path))
    if (inherits(files, "error"))
      next

    hasmeta <- any(grepl("^[^/]+/Meta/?$", files))
    type <- if (hasmeta) "binary" else "source"
    return(type)

  }

  if (!quiet) {
    fmt <- "failed to determine type of package '%s'; assuming source"
    warningf(fmt, aliased_path(path))
  }

  default

}

renv_package_priority <- function(package) {

  # treat 'R' as pseudo base package
  if (package == "R")
    return("base")

  # read priority from db
  db <- renv_installed_packages()
  entry <- db[db$Package == package, ]
  entry$Priority %NA% ""

}

renv_package_tarball_name <- function(path) {
  desc <- renv_description_read(path)
  with(desc, sprintf("%s_%s.tar.gz", Package, Version))
}

renv_package_ext <- function(type) {

  type <- match.arg(type, c("binary", "source"))
  if (type == "source")
    return(".tar.gz")

  switch(
    Sys.info()[["sysname"]],
    Darwin  = ".tgz",
    Windows = ".zip",
    ".tar.gz"
  )

}

renv_packages_base <- function() {
  db <- renv_installed_packages_base()
  c("R", db$Package)
}

renv_package_pkgtypes <- function() {

  # only use binaries if the user has specifically requested it
  # and binaries are available for this installation of R
  # (users may want to install from sources explicitly to take
  # advantage of custom local compiler configurations)
  binaries <-
    !identical(.Platform$pkgType, "source") &&
    !identical(getOption("pkgType"), "source")

  if (binaries) c("binary", "source") else "source"

}

renv_package_augment <- function(installpath, record) {

  # check for remotes fields
  remotes <- record[grep("^Remote", names(record))]
  if (empty(remotes))
    return(FALSE)

  # for backwards compatibility with older versions of Packrat,
  # we write out 'Github*' fields as well
  if (identical(record$Source, "GitHub")) {

    map <- list(
      "GithubHost"     = "RemoteHost",
      "GithubRepo"     = "RemoteRepo",
      "GithubUsername" = "RemoteUsername",
      "GithubRef"      = "RemoteRef",
      "GithubSHA1"     = "RemoteSha"
    )

    enumerate(map, function(old, new) {
      remotes[[old]] <<- remotes[[old]] %||% remotes[[new]]
    })

  }

  # ensure RemoteType field is written out
  remotes$RemoteType <- remotes$RemoteType %||% renv_record_source(record)
  remotes <- remotes[c("RemoteType", renv_vector_diff(names(remotes), "RemoteType"))]

  # update package items
  renv_package_augment_description(installpath, remotes)
  renv_package_augment_metadata(installpath, remotes)

}

renv_package_augment_impl <- function(data, remotes) {
  remotes <- remotes[map_lgl(remotes, Negate(is.null))]
  nonremotes <- grep("^(?:Remote|Github)", names(data), invert = TRUE)
  c(data[nonremotes], remotes)
}

renv_package_augment_description <- function(path, remotes) {

  descpath <- file.path(path, "DESCRIPTION")

  before <- renv_description_read(descpath)
  after <- renv_package_augment_impl(before, remotes)
  if (identical(before, after))
    return(FALSE)

  renv_dcf_write(after, file = descpath)

}

renv_package_augment_metadata <- function(path, remotes) {

  metapath <- file.path(path, "Meta/package.rds")
  meta <- readRDS(metapath)

  before <- as.list(meta$DESCRIPTION)
  after <- renv_package_augment_impl(before, remotes)
  if (identical(before, after))
    return(FALSE)

  meta$DESCRIPTION <- map_chr(after, identity)
  saveRDS(meta, file = metapath, version = 2L)

}

# find recursive dependencies of a package. note that this routine
# doesn't farm out to CRAN; it relies on the package and its dependencies
# all being installed locally. returns a named vector mapping package names
# to the path where they were discovered, or NA if those packages are not
# installed
renv_package_dependencies <- function(packages,
                                      project = NULL,
                                      libpaths = NULL,
                                      fields = NULL)
{
  visited <- new.env(parent = emptyenv())
  ignored <- renv_project_ignored_packages(project = project)
  packages <- renv_vector_diff(packages, ignored)
  libpaths <- libpaths %||% renv_libpaths_all()
  fields <- fields %||% settings$package.dependency.fields(project = project)

  for (package in packages)
    renv_package_dependencies_impl(package, visited, libpaths, fields)

  as.list(visited)
}

renv_package_dependencies_impl <- function(package,
                                           visited,
                                           libpaths = NULL,
                                           fields = NULL)
{
  # skip the 'R' package
  if (package == "R")
    return()

  # if we've already visited this package, bail
  if (exists(package, envir = visited, inherits = FALSE))
    return()

  # default to unknown path for visited packages
  assign(package, NA, envir = visited, inherits = FALSE)

  # find the package
  libpaths <- libpaths %||% renv_libpaths_all()
  location <- renv_package_find(package, libpaths = libpaths)
  if (!file.exists(location))
    return(location)

  # we know the path, so set it now
  assign(package, location, envir = visited, inherits = FALSE)

  # find its dependencies from the DESCRIPTION file
  deps <- renv_dependencies_discover_description(location, fields)
  subpackages <- deps$Package
  for (subpackage in subpackages)
    renv_package_dependencies_impl(subpackage, visited, libpaths, fields)
}

renv_package_reload <- function(package, library = NULL) {
  status <- catch(renv_package_reload_impl(package, library))
  !inherits(status, "error") && status
}

renv_package_reload_impl <- function(package, library) {

  if (renv_testing())
    return(FALSE)

  # record if package is attached (and, if so, where)
  name <- paste("package", package, sep = ":")
  pos <- match(name, search())

  # unload the package
  if (!is.na(pos))
    renv_package_reload_impl_searchpath(package, library, pos)
  else
    renv_package_reload_impl_namespace(package, library)

  TRUE

}

renv_package_reload_impl_searchpath <- function(package, library, pos) {

  args <- list(pos = pos, unload = TRUE, force = TRUE)
  quietly(do.call(base::detach, args), sink = FALSE)

  args <- list(package = package, pos = pos, lib.loc = library, quietly = TRUE)
  quietly(do.call(base::library, args), sink = FALSE)

}

renv_package_reload_impl_namespace <- function(package, library) {
  unloadNamespace(package)
  loadNamespace(package, lib.loc = library)
}
