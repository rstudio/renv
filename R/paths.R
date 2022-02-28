
`_renv_root` <- NULL

renv_paths_override <- function(name) {

  # # check for value from option
  # optname <- paste("renv.paths", name, sep = ".")
  # optval <- getOption(optname)
  # if (!is.null(optval))
  #   return(optval)

  # check for value from envvar
  envname <- paste("RENV_PATHS", toupper(name), sep = "_")
  envval  <- Sys.getenv(envname, unset = NA)
  if (!is.na(envval))
    return(envval)

}

renv_paths_common <- function(name, prefixes = NULL, ...) {

  # check for single absolute path supplied by user
  # TODO: handle multiple?
  end <- file.path(...)
  if (length(end) == 1 && renv_path_absolute(end))
    return(end)

  # check for path provided via option
  root <- renv_paths_override(name) %||% renv_paths_root(name)

  # split path entries containing a separator
  if (name %in% c("cache", "local", "cellar")) {
    pattern <- if (renv_platform_windows()) "[;]" else "[;:]"
    root <- strsplit(root, pattern)[[1L]]
  }

  # form rest of path
  prefixed <- if (length(prefixes))
    file.path(root, paste(prefixes, collapse = "/"))
  else
    root

  file.path(prefixed, ...) %||% ""

}

renv_paths_project <- function(..., project = NULL) {
  project <- renv_project_resolve(project)
  file.path(project, ...) %||% ""
}

renv_paths_library_root <- function(project) {
  renv_bootstrap_library_root(project)
}

renv_paths_library <- function(..., project = NULL) {
  project <- renv_project_resolve(project)
  root <- renv_paths_library_root(project)
  file.path(root, renv_platform_prefix(), ...) %||% ""
}

renv_paths_lockfile <- function(project = NULL) {

  # allow override
  # TODO: profiles?
  override <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  if (!is.na(override)) {
    last <- substr(override, nchar(override), nchar(override))
    if (last %in% c("/", "\\"))
      override <- paste0(override, "renv.lock")
    return(override)
  }

  # otherwise, use default location (location location relative to renv folder)
  project <- renv_project_resolve(project)
  renv <- renv_paths_renv(project = project)
  file.path(dirname(renv), "renv.lock")

}

renv_paths_settings <- function(project = NULL) {
  renv_paths_renv("settings.dcf", project = project)
}

renv_paths_activate <- function(project = NULL) {
  renv_paths_renv("activate.R", profile = FALSE, project = project)
}

renv_paths_renv <- function(..., profile = TRUE, project = NULL) {
  renv_bootstrap_paths_renv(..., profile = profile, project = project)
}

renv_paths_cellar <- function(...) {
  renv_paths_common("cellar", c(), ...)
}

renv_paths_local <- function(...) {
  renv_paths_common("local", c(), ...)
}

renv_paths_source <- function(...) {
  renv_paths_common("source", c(), ...)
}

renv_paths_binary <- function(...) {
  renv_paths_common("binary", c(renv_platform_prefix()), ...)
}

renv_paths_cache <- function(..., version = NULL) {
  platform <- renv_platform_prefix()
  version <- version %||% renv_cache_version()
  renv_paths_common("cache", c(version, platform), ...)
}

renv_paths_rtools <- function(...) {

  root <- renv_paths_override("rtools")
  if (is.null(root)) {
    spec <- renv_rtools_find()
    root <- spec$root
  }

  file.path(spec$root, ...) %||% ""

}

renv_paths_extsoft <- function(...) {
  renv_paths_common("extsoft", c(), ...)
}

renv_paths_mran <- function(...) {
  renv_paths_common("mran", c(), ...)
}



renv_paths_root <- function(...) {
  root <- renv_paths_override("root") %||% renv_paths_root_default()
  file.path(root, ...) %||% ""
}

# nocov start
renv_paths_root_default <- function() {

  # if we have a cached root value, use it
  if (!is.null(`_renv_root`))
    return(`_renv_root`)

  # use tempdir for cache when running tests
  # this check is necessary here to support packages which might use renv
  # during testing (and we don't want those to try to use the user dir)
  checking <- renv_package_checking()

  # compute the root directory
  root <- if (checking)
    renv_paths_root_default_tempdir()
  else
    renv_paths_root_default_impl()

  # cache the value
  renv_binding_replace("_renv_root", root, renv_envir_self())

  # return it
  invisible(`_renv_root`)

}

renv_paths_root_default_impl <- function() {

  # compute known root directories
  roots <- c(
    renv_paths_root_default_impl_v2(),
    renv_paths_root_default_impl_v1()
  )

  # iterate through those roots, finding the first existing
  for (root in roots)
    if (file.exists(root))
      return(root)

  # if none exist, choose the most recent definition
  roots[[1L]]

}

renv_paths_root_default_impl_v2 <- function() {

  # try using tools to get the user directory
  tools <- renv_namespace_load("tools")
  if (is.function(tools$R_user_dir))
    return(tools$R_user_dir("renv", "cache"))

  renv_paths_root_default_impl_v2_fallback()

}

renv_paths_root_default_impl_v2_fallback <- function() {

  # try using our own backfill for older versions of R
  envvars <- c("R_USER_CACHE_DIR", "XDG_CACHE_HOME")
  for (envvar in envvars) {
    root <- Sys.getenv(envvar, unset = NA)
    if (!is.na(root)) {
      path <- file.path(root, "R/renv")
      return(path)
    }
  }

  # use platform-specific default fallbacks
  if (renv_platform_windows())
    file.path(Sys.getenv("LOCALAPPDATA"), "R/cache/R/renv")
  else if (renv_platform_macos())
    "~/Library/Caches/org.R-project.R/R/renv"
  else
    "~/.cache/R/renv"

}

renv_paths_root_default_impl_v1 <- function() {

  base <- switch(
    Sys.info()[["sysname"]],
    Darwin  = Sys.getenv("XDG_DATA_HOME", "~/Library/Application Support"),
    Windows = Sys.getenv("LOCALAPPDATA", Sys.getenv("APPDATA")),
    Sys.getenv("XDG_DATA_HOME", "~/.local/share")
  )

  file.path(base, "renv")

}

renv_paths_root_default_tempdir <- function() {
  temp <- file.path(tempdir(), "renv")
  ensure_directory(temp)
  return(temp)
}

# nocov end

#' Path Customization
#'
#' Access the paths that `renv` uses for global state storage.
#'
#' By default, `renv` collects state into these folders:
#'
#' \tabular{ll}{
#' **Platform** \tab **Location** \cr
#' Linux        \tab `~/.local/share/renv` \cr
#' macOS        \tab `~/Library/Application Support/renv` \cr
#' Windows      \tab `%LOCALAPPDATA%/renv` \cr
#' }
#'
#' For new installations of `renv` using R (>= 4.0.0), `renv` will use
#' [tools::R_user_dir()] to resolve the root directory. If an `renv` root
#' directory has already been created in one of the old locations, that will
#' still be used. This change was made to comply with the CRAN policy
#' requirements of \R packages. By default, these paths resolve as:
#'
#' \tabular{ll}{
#' **Platform** \tab **Location** \cr
#' Linux        \tab `~/.cache/R/renv` \cr
#' macOS        \tab `~/Library/Caches/org.R-project.R/R/renv` \cr
#' Windows      \tab `%LOCALAPPDATA%/R/cache/R/renv` \cr
#' }
#'
#' If desired, this path can be customized by setting the `RENV_PATHS_ROOT`
#' environment variable. This can be useful if you'd like, for example, multiple
#' users to be able to share a single global cache.
#'
#' The various state sub-directories can also be individually adjusted, if so
#' desired (e.g. you'd prefer to keep the cache of package installations on a
#' separate volume). The various environment variables that can be set are
#' enumerated below:
#'
#' \tabular{ll}{
#' \strong{Environment Variable}     \tab \strong{Description} \cr
#' \code{RENV_PATHS_ROOT}            \tab The root path used for global state storage. \cr
#' \code{RENV_PATHS_LIBRARY}         \tab The path to the project library. \cr
#' \code{RENV_PATHS_LIBRARY_ROOT}    \tab The parent path for project libraries. \cr
#' \code{RENV_PATHS_LIBRARY_STAGING} \tab The parent path used for staged package installs. \cr
#' \code{RENV_PATHS_CELLAR}          \tab The cellar, containing local package binaries and sources. \cr
#' \code{RENV_PATHS_SOURCE}          \tab The path containing downloaded package sources. \cr
#' \code{RENV_PATHS_BINARY}          \tab The path containing downloaded package binaries. \cr
#' \code{RENV_PATHS_CACHE}           \tab The path containing cached package installations. \cr
#' \code{RENV_PATHS_PREFIX}          \tab An optional prefix to prepend to the constructed library / cache paths. \cr
#' \code{RENV_PATHS_RTOOLS}          \tab (Windows only) The path to [Rtools](https://cran.r-project.org/bin/windows/Rtools/). \cr
#' \code{RENV_PATHS_EXTSOFT}         \tab (Windows only) The path containing external software needed for compilation of Windows source packages. \cr
#' \code{RENV_PATHS_MRAN}            \tab The path containing MRAN-related metadata. See `vignette("mran", package = "renv")` for more details. \cr
#' }
#'
#' Note that `renv` will append platform-specific and version-specific entries
#' to the set paths as appropriate. For example, if you have set:
#'
#' ```
#' Sys.setenv(RENV_PATHS_CACHE = "/mnt/shared/renv/cache")
#' ```
#'
#' then the directory used for the cache will still depend on the `renv` cache
#' version (e.g. `v2`), the \R version (e.g. `3.5`) and the platform (e.g.
#' `x86_64-pc-linux-gnu`). For example:
#'
#' ```
#' /mnt/shared/renv/cache/v2/R-3.5/x86_64-pc-linux-gnu
#' ```
#'
#' This ensures that you can set a single `RENV_PATHS_CACHE` environment variable
#' globally without worry that it may cause collisions or errors if multiple
#' versions of \R needed to interact with the same cache.
#'
#' If you need to share the same cache with multiple different Linux operating
#' systems, you may want to set the `RENV_PATHS_PREFIX` environment variable
#' to help disambiguate the paths used on Linux. For example, setting
#' `RENV_PATHS_PREFIX = "ubuntu-bionic"` would instruct `renv` to construct a
#' cache path like:
#'
#' ```
#' /mnt/shared/renv/cache/v2/ubuntu-bionic/R-3.5/x86_64-pc-linux-gnu
#' ```
#'
#' If this is required, it's strongly recommended that this environment
#' variable is set in your \R installation's `Renviron.site` file, typically
#' located at `file.path(R.home("etc"), "Renviron.site")`, so that it can be
#' active for any \R sessions launched on that machine.
#'
#' Starting from `renv 0.13.0`, you can also instruct `renv` to auto-generate
#' an OS-specific component to include as part of library and cache paths,
#' by setting the environment variable:
#'
#' ```
#' RENV_PATHS_PREFIX_AUTO = TRUE
#' ```
#'
#' The prefix will be constructed based on fields within the system's
#' `/etc/os-release` file.
#'
#' If reproducibility of a project is desired on a particular machine, it is
#' highly recommended that the `renv` cache of installed packages + binary
#' packages is backed up and persisted, so that packages can be easily restored
#' in the future -- installation of packages from source can often be arduous.
#'
#' If you want these settings to persist in your project, it is recommended that
#' you add these to an appropriate \R startup file. For example, these could be
#' set in:
#'
#' - A project-local `.Renviron`;
#' - The user-level `.Renviron`;
#' - A file at `file.path(R.home("etc"), "Renviron.site")`.
#'
#' Please see ?[Startup] for more details.
#'
#' @section Package Cellar:
#'
#' If your project depends on one or \R packages that are not available in any
#' remote location, you can still provide a locally-available tarball for `renv`
#' to use during restore. By default, these packages should be made available in
#' the folder as specified by the `RENV_PATHS_CELLAR` environment variable. The
#' package sources should be placed in a file at one of these locations:
#'
#' - `${RENV_PATHS_CELLAR}/<package>_<version>.<ext>`
#' - `${RENV_PATHS_CELLAR}/<package>/<package>_<version>.<ext>`
#' - `<project>/renv/cellar/<package>_<version>.<ext>`
#' - `<project>/renv/cellar/<package>/<package>_<version>.<ext>`
#'
#' where `.<ext>` is `.tar.gz` for source packages, or `.tgz` for binaries on
#' macOS and `.zip` for binaries on Windows. During `restore()`, `renv` will
#' search the cellar for a compatible package, and prefer installation with
#' that copy of the package if appropriate.
#'
#' @section Projects:
#'
#' In order to determine whether a package can safely be removed from the cache,
#' `renv` needs to know which projects are using packages from the cache. Since
#' packages may be symlinked from the cache, and symlinks are by nature a one-way
#' link, projects need to also report that they're using the `renv` cache.
#'
#' To accomplish this, whenever `renv` is used with a project, it will record
#' itself as being used within a file located at:
#'
#' - `${RENV_PATHS_ROOT}/projects`
#'
#' This file is list of projects currently using the `renv` cache. With this,
#' `renv` can crawl projects registered with `renv` and use that to determine if
#' any packages within the cache are no longer in use, and can be removed.
#'
#' @rdname paths
#' @name paths
#'
#' @export
#'
#' @examples
#' # get the path to the project library
#' path <- renv::paths$library()
paths <- list(
  root     = renv_paths_root,
  library  = renv_paths_library,
  lockfile = renv_paths_lockfile,
  settings = renv_paths_settings,
  cache    = renv_paths_cache
)
