
# NOTE: intentionally checks library paths before checking loaded namespaces
renv_package_find <- function(package,
                              lib.loc = renv_libpaths_all(),
                              check.loaded = TRUE)
{
  map_chr(
    package,
    renv_package_find_impl,
    lib.loc = lib.loc,
    check.loaded = check.loaded
  )
}

renv_package_find_impl <- function(package,
                                   lib.loc = renv_libpaths_all(),
                                   check.loaded = TRUE)
{
  # if we've been given the path to an existing package, use it as-is
  if (file.exists(file.path(package, "DESCRIPTION")))
    return(normalizePath(package, winslash = "/", mustWork = TRUE))

  # first, look in the library paths
  for (libpath in lib.loc) {
    pkgpath <- file.path(libpath, package)
    descpath <- file.path(pkgpath, "DESCRIPTION")
    if (file.exists(descpath))
      return(pkgpath)
  }

  # if that failed, check to see if it's loaded and use the associated path
  if (check.loaded && package %in% loadedNamespaces()) {
    path <- renv_namespace_path(package)
    if (file.exists(path))
      return(path)
  }

  # failed to find package
  ""
}

renv_package_installed <- function(package, lib.loc = renv_libpaths_all()) {
  paths <- renv_package_find(package, lib.loc, check.loaded = FALSE)
  nzchar(paths)
}

renv_package_available <- function(package) {
  package %in% loadedNamespaces() || renv_package_installed(package)
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

  info <- renv_file_info(path)
  if (is.na(info$isdir))
    stopf("no package at path '%s'", renv_path_aliased(path))

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

    # suppress warnings to avoid issues with e.g.
    # 'skipping pax global extended headers' when
    # using internal tar
    files <- catch(suppressWarnings(method(path)))
    if (inherits(files, "error"))
      next

    hasmeta <- any(grepl("^[^/]+/Meta/?$", files))
    type <- if (hasmeta) "binary" else "source"
    return(type)

  }

  if (!quiet) {
    fmt <- "failed to determine type of package '%s'; assuming source"
    warningf(fmt, renv_path_aliased(path))
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

  # always use '.tar.gz' for source packages
  type <- match.arg(type, c("binary", "source"))
  if (type == "source")
    return(".tar.gz")

  # otherwise, infer appropriate extension based on platform
  case(
    renv_platform_macos()   ~ ".tgz",
    renv_platform_windows() ~ ".zip",
    renv_platform_unix()    ~ ".tar.gz"
  )

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
  remotes[["Remotes"]] <- data[["Remotes"]] %||% remotes[["Remotes"]]
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

  # short-circuit for NA case
  if (length(libpaths) == 1L && is.na(libpaths))
    return()

  # find the package
  libpaths <- libpaths %||% renv_libpaths_all()
  location <- renv_package_find(package, libpaths)
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

  if (renv_tests_running())
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

renv_package_hook <- function(package, hook) {
  if (package %in% loadedNamespaces())
    hook()
  else
    setHook(packageEvent(package, "onLoad"), hook)
}

renv_package_metadata <- function(package) {
  pkgpath <- renv_package_find(package)
  metapath <- file.path(pkgpath, "Meta/package.rds")
  readRDS(metapath)
}

renv_package_shlib <- function(package) {

  pkgpath <- renv_package_find(package)

  pkgname <- basename(package)
  if (pkgname == "data.table")
    pkgname <- "datatable"

  libname <- paste0(pkgname, .Platform$dynlib.ext)
  file.path(pkgpath, "libs", libname)

}

renv_package_built <- function(path) {

  info <- renv_file_info(path)

  # list files in package
  isarchive <- identical(info$isdir, FALSE)
  files <- if (isarchive)
    renv_archive_list(path)
  else
    list.files(path, full.names = TRUE, recursive = TRUE)

  # for a source package, the canonical way to determine if it has already
  # been built is the presence of a 'Packaged:' field in the DESCRIPTION file
  # ('Built:' for binary packages) but we want to avoid the overhead of
  # unpacking the package if at all possible
  pattern <- "/(?:MD5$|INDEX/|Meta/package\\.rds$)"
  matches <- grep(pattern, files)
  if (length(matches) != 0L)
    return(TRUE)

  # if the above failed, then we'll use the contents of the DESCRIPTION file
  descpaths <- grep("/DESCRIPTION$", files, value = TRUE)
  if (length(descpaths) == 0L)
    return(FALSE)

  n <- nchar(descpaths)
  descpath <- descpaths[n == min(n)]
  contents <- if (isarchive)
    renv_archive_read(path, descpath)
  else
    readLines(descpath, warn = FALSE)

  # check for signs it was built
  pattern <- "^(?:Packaged|Built):"
  matches <- grep(pattern, contents)
  if (length(matches) != 0L)
    return(TRUE)

  # does not appear to be a source package
  FALSE

}

renv_package_checking <- function() {
  "CheckExEnv" %in% search() ||
    !is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)) ||
    !is.na(Sys.getenv("_R_CHECK_SIZE_OF_TARBALL_", unset = NA)) ||
    !is.na(Sys.getenv("TESTTHAT", unset = NA))
}

renv_package_unpack <- function(package, path, subdir = "", force = FALSE) {

  # if this isn't an archive, nothing to do
  info <- renv_file_info(path)
  if (identical(info$isdir, TRUE))
    return(path)

  # find DESCRIPTION files in the archive
  descpaths <- renv_archive_find(path, "(?:^|/)DESCRIPTION$")

  # check for a top-level DESCRIPTION file
  # this is done in case the archive has been already been re-packed, so that a
  # package originally located within a sub-directory is now at the top level
  if (!force) {
    descpath <- grep("^[^/]+/DESCRIPTION$", descpaths, perl = TRUE, value = TRUE)
    if (length(descpath))
      return(path)
  }

  # try to resolve the path to the DESCRIPTION file in the archive
  descpath <- if (nzchar(subdir)) {
    pattern <- sprintf("(?:^|/)\\Q%s\\E/DESCRIPTION$", subdir)
    grep(pattern, descpaths, perl = TRUE, value = TRUE)
  } else {
    n <- nchar(descpaths)
    descpaths[n == min(n)]
  }

  # if this failed, error
  if (length(descpath) != 1L) {
    fmt <- "internal error: couldn't find DESCRIPTION file for package '%s' in archive '%s'"
    stopf(fmt, package, path)
  }

  # create extraction directory
  old <- tempfile("renv-package-old-")
  new <- tempfile("renv-package-new-")
  ensure_directory(c(old, new))

  # decompress archive to dir
  renv_archive_decompress(path, exdir = old)

  # rename (without sub-directory)
  oldpath <- file.path(old, dirname(descpath))
  newpath <- file.path(new, package)
  file.rename(oldpath, newpath)

  # use newpath
  newpath

}
