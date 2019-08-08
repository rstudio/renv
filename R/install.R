
#' Install Packages
#'
#' Install one or more \R packages from a variety of remote sources.
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
#' @inheritParams install-params
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # install the latest version of 'digest' from CRAN
#' renv::install("digest")
#'
#' # install an old version of 'digest' from CRAN (using archives)
#' renv::install("digest@0.6.18")
#'
#' # install 'digest' from GitHub (latest dev. version)
#' renv::install("eddelbuettel/digest")
#'
#' # install a package from local sources
#' renv::install("~/path/to/package")
#'
#' }
install <- function(packages,
                    ...,
                    library = NULL,
                    rebuild = FALSE,
                    project = NULL)
{
  renv_scope_error_handler()
  project <- project %||% renv_project()
  library <- library %||% renv_libpaths_default()

  records <- renv_snapshot_r_packages(library = library)

  remotes <- lapply(packages, function(package) {
    case(
      is.list(package)      ~ package,
      is.character(package) ~ renv_remotes_resolve(package)
    )
  })

  packages <- extract_chr(remotes, "Package")
  names(remotes) <- packages
  records[names(remotes)] <- remotes

  rebuild <- case(
    identical(rebuild, TRUE)  ~ packages,
    identical(rebuild, FALSE) ~ character(),
    as.character(rebuild)
  )

  renv_restore_begin(records = records, packages = packages, rebuild = rebuild)
  on.exit(renv_restore_end(), add = TRUE)

  # retrieve packages
  records <- renv_retrieve(packages)
  renv_install(records, library, project)

  # perform auto snapshot
  if (library == renv_paths_library(project = project))
    renv_snapshot_auto(project = project)

  invisible(records)
}

renv_install <- function(records, library, project) {

  # double-check packages for validity (TODO: not yet)
  # if (!renv_install_preflight(records)) {
  #   message("* Operation aborted.")
  #   return(FALSE)
  # }

  # save active library
  renv_global_set("install.library", library)
  on.exit(renv_global_clear("install.library"), add = TRUE)

  # set up a dummy library path for installation
  templib <- renv_tempfile("renv-templib-")
  ensure_directory(templib)
  renv_scope_libpaths(c(templib, renv_libpaths_all()))

  # get error handler
  state <- renv_restore_state()
  handler <- state$handler %||% function(...) {}

  # iterate through records and install
  for (record in records)
    handler(record$Package, renv_install_impl(record, project))

  # migrate packages into true library
  sources <- list.files(templib, full.names = TRUE)
  targets <- file.path(library, basename(sources))
  names(targets) <- sources
  enumerate(targets, renv_file_move, overwrite = TRUE)

  # clear filebacked cache entries
  paths <- file.path(targets, "DESCRIPTION")
  renv_filebacked_clear("DESCRIPTION", paths)

  invisible(TRUE)

}

renv_install_impl <- function(record, project) {

  # figure out whether we can use the cache during install
  library <- renv_global_get("install.library") %||% renv_libpaths_default()
  linkable <-
    settings$use.cache(project = project) &&
    identical(library, renv_paths_library(project = project))

  linker <- if (linkable) renv_file_link else renv_file_copy

  cacheable <-
    renv_record_cacheable(record) &&
    !renv_restore_rebuild_required(record)

  if (cacheable) {

    # check for cache entry and install if there
    cache <- renv_cache_package_path(record)
    if (file.exists(cache))
      return(renv_install_package_cache(record, cache, linker))

  }

  # report that we're about to start installation
  src <- record$Source
  if (tolower(src) == "local")
    src <- "local sources"

  fmt <- "Installing %s [%s] from %s ..."
  with(record, vwritef(fmt, Package, Version, src))

  # otherwise, install
  withCallingHandlers(
    renv_install_package_local(record),
    error = function(e) {
      vwritef("\tFAILED")
      vwritef(e$output)
    }
  )

  binary <-
    endswith(record$Path, ".tgz") ||
    endswith(record$Path, ".zip")

  feedback <- if (binary)
    "installed binary"
  else
    "built from source"

  vwritef("\tOK (%s)", feedback)

  # link into cache
  if (settings$use.cache(project = project))
    renv_cache_synchronize(record, linkable = linkable)

}

renv_install_package_cache <- function(record, cache, linker) {

  if (renv_install_package_cache_skip(record, cache))
    return(TRUE)

  library <- renv_libpaths_default()
  target <- file.path(library, record$Package)

  # back up the previous installation if needed
  callback <- renv_file_backup(target)
  on.exit(callback(), add = TRUE)

  # report successful link to user
  fmt <- "Installing %s [%s] ..."
  with(record, vwritef(fmt, Package, Version))

  status <- catch(linker(cache, target))
  if (inherits(status, "error"))
    return(status)

  type <- case(
    identical(linker, renv_file_copy) ~ "copied",
    identical(linker, renv_file_link) ~ "linked"
  )

  vwritef("\tOK (%s cache)", type)

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
  callback <- renv_file_backup(destination)
  on.exit(callback(), add = TRUE)

  # install the package
  renv_install_package_local_impl(package, path, library)

  # augment package metadata after install
  installpath <- file.path(library, package)
  renv_package_augment(installpath, record)

  # return the path to the package
  invisible(installpath)

}

renv_install_package_local_impl <- function(package, path, library) {
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  r_cmd_install(package, path, library)
}

renv_install_package_options <- function(package) {
  options <- getOption("renv.install.package.options")
  options[[package]]
}

# nocov start
renv_install_preflight <- function(records) {

  deps <- bapply(records, function(record) {
    renv_dependencies_discover_description(record$Path)
  }, index = "ParentPackage")

  splat <- split(deps, deps$Package)
  bad <- enumerate(splat, function(package, requirements) {

    # skip NULL records (should be handled above)
    record <- records[[package]]
    if (is.null(record))
      return(NULL)

    version <- record$Version

    # drop packages without explicit version requirement
    requirements <- requirements[nzchar(requirements$Require), ]
    if (nrow(requirements) == 0)
      return(NULL)

    # add in requested version
    requirements$RequestedVersion <- version

    # generate expressions to evaluate
    fmt <- "package_version('%s') %s package_version('%s')"
    code <- with(requirements, sprintf(fmt, RequestedVersion, Require, Version))
    parsed <- parse(text = code)
    ok <- map_lgl(parsed, eval, envir = baseenv())

    # return requirements that weren't satisfied
    requirements[!ok, ]

  })

  bad <- bind_list(unname(bad))
  if (empty(bad))
    return(TRUE)

  package  <- bad$ParentPackage
  requires <- sprintf("%s (%s %s)", bad$Package, bad$Require, bad$Version)
  actual   <- sprintf("%s %s", bad$Package, bad$RequestedVersion)

  fmt <- "Package '%s' requires '%s', but '%s' will be installed"
  text <- sprintf(fmt, format(package), format(requires), format(actual))
  if (renv_verbose()) {
    renv_pretty_print(
      text,
      "The following issues were discovered during installation:",
      "Installation of these packages may not succeed.",
      wrap = FALSE
    )
  }

  if (interactive() && !proceed())
    return(FALSE)

  TRUE

}
# nocov end
