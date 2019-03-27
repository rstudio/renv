
#' Install Packages
#'
#' Install one or more \R packages.
#'
#' `install()` uses the same machinery as [restore()] when installing packages.
#' In particular, this means that the local cache of package installations is
#' used when possible. This helps to avoid re-downloading packages that have
#' already been downloaded before, and re-compiling packages from source when
#' a binary copy of that package is already available.
#'
#' Note that this interface is subject to change -- the goal is to hook into
#' separate package installation backends in the future.
#'
#' @inheritParams renv-params
#'
#' @param packages A character vector of \R packages to install. Required
#'   package dependencies (`Depends`, `Imports`, `LinkingTo`) will be installed
#'   as required.
#'
#' @export
install <- function(packages, project = NULL) {
  project <- project %||% renv_project()

  # create lockfile based on state of R libraries
  records <- renv_snapshot_r_packages()
  renv_restore_begin(records, packages)
  on.exit(renv_restore_end(), add = TRUE)

  # retrieve packages
  records <- renv_retrieve(packages, records)
  records <- Filter(renv_install_required, records)
  renv_install(project, records)

  renv_snapshot_auto(project = project)
  invisible(records)
}

renv_install <- function(project, records) {

  # save active library
  library <- renv_libpaths_default()
  renv_global_set("install.library", library)
  on.exit(renv_global_clear("install.library"), add = TRUE)

  # set up a dummy library path for installation
  templib <- tempfile("renv-templib-")
  ensure_directory(templib)
  on.exit(unlink(templib), add = TRUE)
  renv_scope_libpaths(c(templib, .libPaths()))

  # figure out whether we can use the cache during install
  linkable <-
    settings$use.cache() &&
    identical(library, renv_paths_library(project = project))

  linker <- if (linkable) renv_file_link else renv_file_copy

  # iterate through records and install
  for (record in records)
    renv_install_impl(record, linker)

  # migrate packages into true library
  sources <- list.files(templib, full.names = TRUE)
  targets <- file.path(library, basename(sources))
  names(targets) <- sources
  enumerate(targets, renv_file_move, overwrite = TRUE)

}

renv_install_impl <- function(record, linker = renv_file_copy) {

  # check for cache entry and install if there
  cache <- renv_cache_package_path(record)
  if (file.exists(cache))
    return(renv_install_package_cache(record, cache, linker))

  # report that we're about to start installation
  fmt <- "Installing %s [%s] from %s ..."
  with(record, messagef(fmt, Package, Version, renv_alias(Source)))

  # otherwise, install
  status <- catch(renv_install_package_local(record))
  renv_install_report_status(record, status)

}

renv_install_package_cache <- function(record, cache, linker) {

  if (renv_install_package_cache_skip(record, cache))
    return(TRUE)

  library <- renv_libpaths_default()
  target <- file.path(library, record$Package)

  # back up the previous installation if needed
  callback <- renv_file_scoped_backup(target)
  on.exit(callback(), add = TRUE)

  # report successful link to user
  fmt <- "Installing %s [%s] ..."
  with(record, messagef(fmt, Package, Version))

  status <- catch(linker(cache, target))
  if (inherits(status, "error"))
    return(status)

  type <- case(
    identical(linker, renv_file_copy) ~ "copied",
    identical(linker, renv_file_link) ~ "linked"
  )

  messagef("\tOK (%s cache)", type)

  return(TRUE)

}

renv_install_package_cache_skip <- function(record, cache) {

  state <- renv_restore_state()

  # don't skip if installation was explicitly requested
  if (record$Package %in% state$packages)
    return(FALSE)

  # check for matching cache + target paths
  library <- renv_global_get("install.library")
  target <- file.path(library, record$Package)

  renv_file_same(cache, target)

}

renv_install_package_local <- function(record, quiet = TRUE) {

  package <- record$Package

  # get user-defined options to apply during installation
  options <- renv_install_package_options(package)

  # run user-defined hooks before, after install
  before <- options$before.install %||% identity
  after  <- options$after.install %||% identity

  before(package)
  on.exit(after(package), add = TRUE)

  library <- renv_libpaths_default()
  path <- record$Path

  destination <- file.path(library, package)
  callback <- renv_file_scoped_backup(destination)
  on.exit(callback(), add = TRUE)

  renv_install_package_local_impl(package, path, library)

}

renv_install_package_local_impl <- function(package, path, library) {

  # prepare library, package paths
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # set up arguments
  args <- c("CMD", "INSTALL", "-l", shQuote(library), shQuote(path))
  rlibs <- paste(renv_libpaths_all(), collapse = .Platform$path.sep)
  env <- paste("R_LIBS", shQuote(rlibs), sep = "=")

  # do the install
  output <- suppressWarnings(
    system2(R(), args, stdout = TRUE, stderr = TRUE, env = env)
  )

  # check for successful install
  status <- attr(output, "status") %||% 0L
  if (identical(status, 0L))
    return(TRUE)

  # installation failed; write output for user
  header <- sprintf("Error installing package '%s':", package)
  lines <- paste(rep("=", nchar(header)), collapse = "")
  all <- c(header, lines, "", output)
  vwritef(paste(all, collapse = "\n"), con = stderr())

  # stop with an error
  stopf("installation of package '%s' failed", package)

}

renv_install_package_options <- function(package) {
  options <- getOption("renv.install.package.options")
  options[[package]]
}

renv_install_report_status <- function(record, status) {

  if (inherits(status, "error")) {
    message("\tFAILED")
    stop(status)
  }

  feedback <- case(
    endswith(".tar.gz", record$Path) ~ "built from source",
    "installed binary"
  )

  messagef("\tOK (%s)", feedback)

  if (settings$use.cache())
    renv_cache_synchronize(record)

  return(TRUE)

}

# NOTE: this routine does a very primitive sort of dependency validation;
# it simply checks if the package is too old and requests an install
# if that's the case. e.g. if pkg A requires pkgB >= 1.1, but pkgB 1.0
# is installed, then this routine marks pkgB as requiring install
renv_install_required <- function(record) {
  state <- renv_restore_state()

  # if installation of this package was explicitly requested, keep it
  package <- record$Package
  if (package %in% state$packages)
    return(TRUE)

  # check to see if this package is already installed; if it's not
  # installed then we need to install it
  records <- state$records
  if (is.null(records[[record$Package]]))
    return(TRUE)

  # check and see if the installed version satisfies all requirements
  requirements <- state$requirements[[package]]
  if (is.null(requirements))
    return(FALSE)

  data <- bind_list(requirements$data())
  explicit <- data[nzchar(data$Require) & nzchar(data$Version), ]
  if (nrow(explicit) == 0)
    return(FALSE)

  exprs <- sprintf(
    "numeric_version('%s') %s '%s'",
    record$Version,
    explicit$Require,
    explicit$Version
  )

  expr <- paste(exprs, collapse = " && ")
  satisfied <- catch(eval(parse(text = expr)), envir = baseenv())
  if (inherits(satisfied, "error"))
    warning(satisfied)

  !identical(satisfied, TRUE)

}
