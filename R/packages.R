
renv_package_installed <- function(package, lib.loc = NULL) {
  location <- find.package(package, lib.loc = lib.loc, quiet = TRUE)
  length(location) > 0
}

renv_package_version <- function(package) {
  renv_package_description_field(package, "Version")
}

renv_package_description_field <- function(package, field) {
  desc <- renv_description_read(package = package)
  desc[[field]]
}

renv_package_type <- function(path) {

  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("no package at path '%s'", aliased_path(path))

  # for directories, assume it's a source package
  if (info$isdir)
    return("source")

  # otherwise, guess based on contents of package
  methods <- list(
    function(path) untar(tarfile = path, list = TRUE),
    function(path) unzip(zipfile = path, list = TRUE)$Name
  )

  for (method in methods) {

    files <- catch(method(path))
    if (inherits(files, "error"))
      next

    if (any(grepl("^[^/]+/Meta/?$", files)))
      return("binary")
    else
      return("source")

  }

  # all else fails, assume source
  fmt <- "failed to determine type of package '%s'; assuming source"
  warningf(fmt, aliased_path(path))

  "source"

}

renv_package_priority <- function(package) {

  # treat 'R' as pseudo base package
  if (package == "R")
    return("base")

  # read priority from db
  db <- renv_installed_packages()
  entry <- db[package, ]
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

  # ensure RemoteType field is written out
  remotes$RemoteType <- remotes$RemoteType %||% tolower(record$Source)
  remotes <- remotes[c("RemoteType", setdiff(names(remotes), "RemoteType"))]

  # update package items
  renv_package_augment_description(installpath, remotes)
  renv_package_augment_metadata(installpath, remotes)

}

renv_package_augment_impl <- function(data, remotes) {
  remotes <- remotes[map_lgl(remotes, nzchar)]
  nonremotes <- grep("^Remote", names(data), invert = TRUE)
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
renv_package_dependencies <- function(project,
                                      packages,
                                      libpaths = NULL,
                                      fields = NULL)
{
  visited <- new.env(parent = emptyenv())
  ignored <- c("renv", settings$ignored.packages(project = project))
  packages <- setdiff(packages, ignored)
  libpaths <- libpaths %||% renv_libpaths_all()
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
    renv_package_dependencies_impl(subpackage, visited)
}
