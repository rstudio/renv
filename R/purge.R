
#' Purge Packages from the Cache
#'
#' Purge packages from the cache. This can be useful if a package which had
#' previously been installed in the cache has become corrupted or unusable,
#' and needs to be reinstalled.
#'
#' `purge()` is an inherently destructive option. It removes packages from the
#' cache, and so any project which had symlinked that package into its own
#' project library would find that package now unavailable. These projects would
#' hence need to reinstall any purged packages. Take heed of this in case you're
#' looking to purge the cache of a package which is difficult to install, or
#' if the original sources for that package are no longer available!
#'
#' @inheritParams renv-params
#'
#' @param package A single package to be removed from the cache.
#' @param version The package version to be removed. When `NULL`, all versions
#'   of the requested package will be removed.
#' @param hash The specific hashes to be removed. When `NULL`, all hashes
#'   associated with a particular package's version will be removed.
#'
#' @export
purge <- function(package,
                  version = NULL,
                  hash = NULL,
                  confirm = interactive())
{
  invisible(renv_purge_impl(package, version, hash, confirm))
}

renv_purge_impl <- function(package,
                            version = NULL,
                            hash = NULL,
                            confirm = interactive())
{
  if (length(package) != 1)
    stop("argument 'package' is not of length one", call. = FALSE)

  bail <- function() {
    vwritef("* The requested package is not installed in the cache -- nothing to do.")
    character()
  }

  # get root cache path entry for package
  paths <- renv_paths_cache(package)
  if (!file.exists(paths))
    return(bail())

  # construct versioned path
  paths <- if (is.null(version))
    list.files(paths, full.names = TRUE)
  else
    file.path(paths, version)
  if (all(!file.exists(paths)))
    return(bail())

  # construct hashed path
  paths <- if (is.null(hash))
    list.files(paths, full.names = TRUE)
  else
    file.path(paths, hash)
  if (all(!file.exists(paths)))
    return(bail())

  # check that these entries exist
  missing <- !file.exists(paths)
  if (any(missing)) {

    renv_pretty_print(
      paths[missing],
      "The following entries were not found in the cache:",
      "They will be ignored.",
      wrap = FALSE
    )

    paths <- paths[!missing]

  }

  if (confirm || renv_verbose()) {

    fmt <- "%s %s [%s]"
    package <- path_component(paths, 3)
    version <- path_component(paths, 2)
    entries <- sprintf(fmt, package, version, aliased_path(paths))

    renv_pretty_print(
      entries,
      "The following packages will be purged from the cache:",
      wrap = FALSE
    )

    if (confirm && !proceed()) {
      message("* Operation aborted.")
      return(paths)
    }

  }

  unlink(paths, recursive = TRUE)
  printf("* Removed %i packages.", length(paths))
  invisible(paths)

}
