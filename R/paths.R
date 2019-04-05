
#' Path Customization
#'
#' Customize the paths that `renv` uses for global state storage.
#'
#' By default, `renv` collects state into these folders:
#'
#' \tabular{ll}{
#' **Platform** \tab **Location** \cr
#' Linux        \tab `~/.local/share/renv` \cr
#' macOS        \tab `~/Library/Application Support/renv` \cr
#' Windows      \tab `%APPDATA%/renv` \cr
#' }
#'
#' If desired, this path can be adjusted by setting the `RENV_PATHS_ROOT`
#' environment variable. This can be useful if you'd like, for example, multiple
#' users to be able to share a single global cache.
#'
#' The various state sub-directories can also be individually adjusted, if so
#' desired (e.g. you'd prefer to keep the cache of package installations on a
#' separate volume). The various environment variables that can be set are
#' enumerated below:
#'
#' \tabular{ll}{
#' \strong{Environment Variable} \tab \strong{Description} \cr
#' \code{RENV_PATHS_ROOT}        \tab The root path used for global state storage. \cr
#' \code{RENV_PATHS_BOOTSTRAP}   \tab The library path containing bootstrapped `renv` installations. \cr
#' \code{RENV_PATHS_LIBRARY}     \tab The root path containing different \R libraries. \cr
#' \code{RENV_PATHS_SOURCE}      \tab The path containing downloaded package sources. \cr
#' \code{RENV_PATHS_BINARY}      \tab The path containing downloaded package binaries. \cr
#' \code{RENV_PATHS_CACHE}       \tab The path containing cached package installations. \cr
#' \code{RENV_PATHS_REPOS}       \tab The path containing cached available package information. \cr
#' \code{RENV_PATHS_EXTSOFT}     \tab (Windows only) The path containing external software needed for compilation of Windows source packages. \cr
#' }
#'
#' If reproducibility of a project is desired on a particular machine, it is
#' highly recommended that the `renv` cache of installed packages + binary
#' packages is stored, so that packages can be easily restored in the future --
#' installation of packages from source can often be arduous.
#'
#' If you want these settings to persist in your project, it is recommended that
#' you add these to an appropriate \R startup file. For example, these could be
#' set in:
#'
#' - A project-local `.Renviron`;
#' - The user-level `.Renviron`;
#' - A file at `$(R RHOME)/etc/Renviron.site`.
#'
#' Please see ?[Startup] for more details.
#'
#' @rdname paths
#' @name paths
NULL

renv_paths_common <- function(name, root, prefix, ...) {

  # allow explicit absolute paths from the user
  suffix <- file.path(...)
  if (length(suffix) && path_absolute(suffix))
    return(suffix)

  # prepend prefix (if any)
  if (prefix)
    suffix <- renv_platform_prefix(...)

  # get root path
  envvar <- sprintf("RENV_PATHS_%s", toupper(name))
  root <- Sys.getenv(envvar, unset = root(name))

  if (empty(suffix))
    root
  else
    file.path(root, suffix) %||% ""

}

renv_paths_common_local <- function(project = NULL, name, prefix, ...) {
  project <- project %||% renv_project()
  root <- function(...) file.path(project, "renv", ...) %||% ""
  renv_paths_common(name, root, prefix, ...)
}

renv_paths_library <- function(project = NULL, ...) {
  renv_paths_common_local(project, "library", TRUE, ...)
}

renv_paths_source <- function(...) {
  renv_paths_common("source", renv_paths_root, FALSE, ...)
}

renv_paths_bootstrap <- function(...) {
  renv_paths_common("bootstrap", renv_paths_root_versioned, TRUE, ...)
}

renv_paths_binary <- function(...) {
  renv_paths_common("binary", renv_paths_root_versioned, FALSE, ...)
}

renv_paths_repos <- function(...) {
  renv_paths_common("repos", renv_paths_root_versioned, FALSE, ...)
}

renv_paths_cache <- function(...) {
  renv_paths_common("cache", renv_paths_root_versioned, FALSE, renv_cache_version(), ...)
}

renv_paths_extsoft <- function(...) {
  drive <- Sys.getenv("SYSTEMDRIVE", unset = "C:")
  root <- file.path(drive, "RBuildTools/extsoft")
  file.path(root, ...) %||% ""
}


renv_paths_root <- function(...) {
  root <- Sys.getenv("RENV_PATHS_ROOT", renv_paths_root_default())
  file.path(root, ...) %||% ""
}

renv_paths_root_versioned <- function(...) {
  version <- paste("R", getRversion()[1, 1:2], sep = "-")
  renv_paths_root(version, ...) %||% ""
}

renv_paths_root_default <- function() {
  renv_global("root", renv_paths_root_default_impl())
}

renv_paths_root_default_impl <- function() {

  root <- switch(
    Sys.info()[["sysname"]],
    Darwin  = Sys.getenv("XDG_DATA_HOME", "~/Library/Application Support"),
    Windows = Sys.getenv("LOCALAPPDATA", "~/.renv"),
    Sys.getenv("XDG_DATA_HOME", "~/.local/share")
  )

  file.path(root, "renv")

}

renv_platform_prefix <- function(...) {
  file.path(R.version$platform, getRversion()[1, 1:2], ...)
}

