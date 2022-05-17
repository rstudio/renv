
#' Isolate a Project
#'
#' Copy packages from the `renv` cache directly into the project library, so
#' that the project can continue to function independently of the `renv` cache.
#'
#' After calling `isolate()`, `renv` will still be able to use the cache on
#' future [install()]s and [restore()]s. If you'd prefer that `renv` copy
#' packages from the cache, rather than use symlinks, you can set the `renv`
#' configuration option:
#'
#' ```
#' options(renv.config.cache.symlinks = FALSE)
#' ```
#'
#' to force `renv` to copy packages from the cache, as opposed to symlinking
#' them. If you'd like to disable the cache altogether for a project, you can
#' use:
#'
#' ```
#' settings$use.cache(FALSE)
#' ```
#'
#' to explicitly disable the cache for the project.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # isolate a project
#' renv::isolate()
#'
#' }
isolate <- function(project = NULL) {

  project <- renv_project_resolve(project)
  renv_scope_lock(project = project)

  if (renv_platform_windows())
    renv_isolate_windows(project)
  else
    renv_isolate_unix(project)

  invisible(project)

}

renv_isolate_unix <- function(project) {

  library <- renv_paths_library(project = project)
  targets <- list.files(library, full.names = TRUE)

  sources <- Sys.readlink(targets)
  islink <- !is.na(sources) & nzchar(sources)

  sources <- sources[islink]
  targets <- targets[islink]
  names(targets) <- sources

  if (length(targets)) {
    vprintf("* Copying packages into the private library ... ")
    unlink(targets)
    copy <- renv_progress_callback(renv_file_copy, length(targets))
    enumerate(targets, copy, overwrite = TRUE)
    vwritef("Done!")
  }

  writef("* This project has been isolated from the cache.")
  invisible(project)

}

renv_isolate_windows <- function(project) {

  library <- renv_paths_library(project = project)
  targets <- list.files(library, full.names = TRUE)

  sources <- map_chr(targets, renv_cache_path)
  names(targets) <- sources

  if (length(targets)) {
    vprintf("* Copying packages into the private library ... ")
    targets <- targets[file.exists(sources)]
    unlink(targets)
    copy <- renv_progress_callback(renv_file_copy, length(targets))
    enumerate(targets, copy, overwrite = TRUE)
    vwritef("Done!")
  }

  writef("* This project has been isolated from the cache.")
  invisible(project)

}
