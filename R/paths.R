
renv_prefix_platform <- function() {
  renv_bootstrap_prefix()
}

renv_paths_common <- function(name, prefixes = NULL, ...) {

  # check for single absolute path supplied by user
  # TODO: handle multiple?
  end <- file.path(...)
  if (length(end) == 1 && renv_path_absolute(end))
    return(end)

  # compute root path
  envvar <- paste("RENV_PATHS", toupper(name), sep = "_")
  root <-
    Sys.getenv(envvar, unset = NA) %NA%
    renv_paths_root(name)

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
  file.path(root, renv_prefix_platform(), ...) %||% ""
}

renv_paths_local <- function(...) {
  renv_paths_common("local", c(), ...)
}

renv_paths_source <- function(...) {
  renv_paths_common("source", c(), ...)
}

renv_paths_binary <- function(...) {
  renv_paths_common("binary", c(renv_prefix_platform()), ...)
}

renv_paths_cache <- function(..., version = NULL) {
  platform <- renv_prefix_platform()
  version <- version %||% renv_cache_version()
  renv_paths_common("cache", c(version, platform), ...)
}

renv_paths_rtools <- function(...) {

  root <- Sys.getenv("RENV_PATHS_RTOOLS", unset = NA)
  if (!is.na(root))
    return(root)

  # TODO: this was a typo in a previous usage; preserved
  # for backwards compatibility
  root <- Sys.getenv("RENV_PATH_RTOOLS", unset = NA)
  if (!is.na(root))
    return(root)

  spec <- renv_rtools_find()
  file.path(spec$root, ...) %||% ""

}

renv_paths_extsoft <- function(...) {
  renv_paths_common("extsoft", c(), ...)
}

renv_paths_mran <- function(...) {
  renv_paths_common("mran", c(), ...)
}



renv_paths_root <- function(...) {

  root <-
    Sys.getenv("RENV_PATHS_ROOT", unset = NA) %NA%
    renv_paths_root_default()

  file.path(root, ...) %||% ""

}

# nocov start
renv_paths_root_default <- function() {

  root <- switch(
    Sys.info()[["sysname"]],
    Darwin  = Sys.getenv("XDG_DATA_HOME", "~/Library/Application Support"),
    Windows = Sys.getenv("LOCALAPPDATA", Sys.getenv("APPDATA")),
    Sys.getenv("XDG_DATA_HOME", "~/.local/share")
  )

  root <- renv_path_normalize(root, winslash = "/", mustWork = FALSE)
  path <- file.path(root, "renv")

  # check for user consent
  consenting <- identical(getOption("renv.consenting"), TRUE)
  if (consenting)
    return(path)

  consent <- identical(getOption("renv.consent"), TRUE)
  if (consent) {
    ensure_directory(path)
    return(path)
  }

  # if this root directory has not yet been created, then use a path
  # in the temporary directory instead (users must call renv::consent
  # to create this path explicitly)
  type <- renv_file_type(path, symlinks = FALSE)
  if (type == "directory")
    return(path)

  if (renv_once()) {
    renv_pretty_print(
      aliased_path(path),
      "The renv cache root has not yet been created:",
      c(
        "A temporary cache directory will be used instead.",
        "Please call `renv::consent()` to allow renv to generate a cache directory.",
        "Please restart the R session after providing consent."
      ),
      wrap = FALSE
    )
  }

  temp <- tempfile("renv-root-")
  ensure_directory(temp)
  return(temp)

}
# nocov end

renv_paths_init <- function() {

  envvars <- Sys.getenv()

  keys <- grep("^RENV_PATHS_", names(envvars), value = TRUE)
  if (empty(keys))
    return(character())

  args <- lapply(envvars[keys], normalizePath, winslash = "/", mustWork = FALSE)
  do.call(Sys.setenv, args)

}

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
#' \strong{Environment Variable}  \tab \strong{Description} \cr
#' \code{RENV_PATHS_ROOT}         \tab The root path used for global state storage. \cr
#' \code{RENV_PATHS_LIBRARY}      \tab The path to the project library. \cr
#' \code{RENV_PATHS_LIBRARY_ROOT} \tab The parent path for project libraries. \cr
#' \code{RENV_PATHS_LOCAL}        \tab The path containing local package sources. \cr
#' \code{RENV_PATHS_SOURCE}       \tab The path containing downloaded package sources. \cr
#' \code{RENV_PATHS_BINARY}       \tab The path containing downloaded package binaries. \cr
#' \code{RENV_PATHS_CACHE}        \tab The path containing cached package installations. \cr
#' \code{RENV_PATHS_PREFIX}       \tab An optional prefix to prepend to the constructed library / cache paths. \cr
#' \code{RENV_PATHS_RTOOLS}       \tab (Windows only) The path to [Rtools](https://cran.r-project.org/bin/windows/Rtools/). \cr
#' \code{RENV_PATHS_EXTSOFT}      \tab (Windows only) The path containing external software needed for compilation of Windows source packages. \cr
#' \code{RENV_PATHS_MRAN}         \tab The path containing MRAN-related metadata. See `vignette("mran", package = "renv")` for more details. \cr
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
#' @section Local Sources:
#'
#' If your project depends on one or \R packages that are not available in any
#' remote location, you can still provide a locally-available tarball for `renv`
#' to use during restore. By default, these packages should be made available in
#' the folder as specified by the `RENV_PATHS_LOCAL` environment variable. The
#' package sources should be placed in a file at one of these locations:
#'
#' - `${RENV_PATHS_LOCAL}/<package>_<version>.<ext>`
#' - `${RENV_PATHS_LOCAL}/<package>/<package>_<version>.<ext>`
#' - `<project>/renv/local/<package>_<version>.<ext>`
#' - `<project>/renv/local/<package>/<package>_<version>.<ext>`
#'
#' where `.<ext>` is `.tar.gz` for source packages, or `.tgz` for binaries on
#' macOS and `.zip` for binaries on Windows. During a `restore()`, packages
#' installed from an unknown source will be searched for in this location.
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
  root    = renv_paths_root,
  library = renv_paths_library,
  cache   = renv_paths_cache
)
