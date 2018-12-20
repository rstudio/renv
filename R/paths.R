
#' Path Customization
#'
#' Customize the paths that `renv` uses for global state storage.
#'
#' By default, all state is collected into a directory at `~/.renv`. This
#' directory can be adjusted by setting the `RENV_PATHS_ROOT` environment
#' variable. If desired, this path can be adjusted -- this can be useful if
#' you want to share a set of environments with multiple users.
#'
#' The various state sub-directories can also be individually adjusted, if so
#' desired (e.g. you'd prefer to keep the larger 'library' directory on a
#' separate volume). The various environment variables that can be set are
#' enumerated below:
#'
#' \tabular{ll}{
#' \strong{Environment Variable} \tab \strong{Description} \cr
#' \code{RENV_PATHS_ROOT}        \tab The root path used for global state storage. \cr
#' \code{RENV_PATHS_BOOTSTRAP}   \tab The library path containing bootstrapped `renv` installations. \cr
#' \code{RENV_PATHS_LIBRARY}     \tab The root path containing different \R libraries. \cr
#' \code{RENV_PATHS_ENVIRONMENT} \tab The path containing \R virtual environment definitions. \cr
#' \code{RENV_PATHS_CACHE}       \tab The path containing cached package sources, binaries, and installs. \cr
#' }
#'
#' If you want these settings to persist in your project, it is recommended
#' that you add these to a project-local `.Renviron`.
#'
#' @rdname paths
#' @name paths
NULL

renv_paths_bootstrap <- function(...) {
  root <- Sys.getenv("RENV_PATHS_BOOTSTRAP", renv_paths_root_local("bootstrap"))
  file.path(root, renv_platform_prefix(), ...) %||% ""
}

renv_paths_environment <- function(...) {
  root <- Sys.getenv("RENV_PATHS_ENVIRONMENT", renv_paths_root_local("environment"))
  file.path(root, ...) %||% ""
}

renv_paths_library <- function(...) {
  root <- Sys.getenv("RENV_PATHS_LIBRARY", renv_paths_root_local("library"))
  file.path(root, renv_platform_prefix(), ...) %||% ""
}

renv_paths_cache <- function(...) {
  root <- Sys.getenv("RENV_PATHS_CACHE", renv_paths_root("cache"))
  file.path(root, ...) %||% ""
}



renv_paths_root <- function(...) {
  root <- Sys.getenv("RENV_PATHS_ROOT", renv_paths_root_default())
  file.path(root, ...) %||% ""
}

renv_paths_root_local <- function(...) {
  if (renv_local())
    file.path(renv_active_project(), "renv", ...)
  else
    renv_paths_root(...)
}

renv_paths_root_default <- function() {
  if (is_rcmd_check())
    renv_global("r.cmd.check.root", tempfile("renv-root-"))
  else
    path.expand("~/.renv")
}


renv_platform_prefix <- function(...) {
  file.path(R.version$platform, getRversion()[1, 1:2])
}

