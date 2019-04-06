
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
#' Note that `renv` will append platform-specific and version-specific entries
#' to the set paths as required. For example, if you have set:
#'
#'     Sys.setenv(RENV_PATHS_CACHE = "/mnt/shared/renv/cache")
#'
#' then the directory used for the cache will still depend on the \R version
#' and also cache
#'
#'     /mnt/shared/renv/cache/R-3.5/v2
#'
#' This ensures that you can set a single `RENV_PATHS_CACHE` environment variable
#' globally without worry that it may cause collisions or errors if multiple
#' versions of \R needed to interact with the same cache.
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

renv_prefix_platform <- function() {
  file.path(R.version$platform, getRversion()[1, 1:2])
}

renv_prefix_version <- function() {
  paste("R", getRversion()[1, 1:2], sep = "-")
}

renv_paths_common <- function(name, prefixes = NULL, ...) {

  # check for absolute path
  suffix <- file.path(...)
  if (length(suffix) && path_absolute(suffix))
    return(suffix)

  # compute root path
  envvar <- paste("RENV_PATHS", toupper(name), sep = "_")
  root <- Sys.getenv(envvar, unset = renv_paths_root(name))

  # form rest of path
  components <- as.list(c(root, prefixes, ...))
  do.call(file.path, components)

}

renv_paths_library <- function(..., project = NULL) {
  project <- project %||% renv_project()
  root <- Sys.getenv("RENV_PATHS_LIBRARY", unset = file.path(project, "renv/library"))
  file.path(root, renv_prefix_platform(), ...)
}

renv_paths_source <- function(...) {
  renv_paths_common("source", ...)
}

renv_paths_bootstrap <- function(...) {
  renv_paths_common("bootstrap", c(renv_prefix_version(), renv_prefix_platform()), ...)
}

renv_paths_binary <- function(...) {
  renv_paths_common("binary", renv_prefix_version(), ...)
}

renv_paths_repos <- function(...) {
  renv_paths_common("repos", renv_prefix_version(), ...)
}

renv_paths_cache <- function(...) {
  renv_paths_common("cache", c(renv_prefix_version(), renv_cache_version()), ...)
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
