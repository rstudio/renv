
#' Upgrade renv
#'
#' @description
#' Upgrade the version of renv associated with a project, including using
#' a development version from GitHub. Automatically snapshots the update
#' renv, updates the activate script, and restarts R.
#'
#' If you want to update all packages (including renv) to their latest CRAN
#' versions, use [renv::update()].
#'
#' @inherit renv-params
#'
#' @param version The version of renv to be installed.
#'
#'   When `NULL` (the default), the latest version of renv will be installed as
#'   available from CRAN (or whatever active package repositories are active)
#'   Alternatively, you can install the latest development version with
#'  `"main"`, or a specific commit with a SHA, e.g. `"5049cef8a"`.
#'
#' @param prompt Boolean; prompt upgrade before proceeding?
#'
#' @param reload Boolean; reload renv after install? When `NULL` (the
#'   default), renv will be re-loaded only if updating renv for the
#'   active project. Since it's not possible to guarantee a clean reload
#'   in the current session, this will attempt to restart your R session.
#'
#' @return A boolean value, indicating whether the requested version of
#'   renv was successfully installed. Note that this function is normally
#'   called for its side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # upgrade to the latest version of renv
#' renv::upgrade()
#'
#' # upgrade to the latest version of renv on GitHub (development version)
#' renv::upgrade(version = "main")
#'
#' }
upgrade <- function(project = NULL,
                    version = NULL,
                    reload  = NULL,
                    prompt = interactive())
{
  renv_scope_error_handler()
  renv_scope_verbose_if(prompt)
  invisible(renv_upgrade_impl(project, version, reload, prompt))
}

renv_upgrade_impl <- function(project, version, reload, prompt) {

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  reload <- reload %||% renv_project_loaded(project)

  lockfile <- renv_lockfile_load(project)
  old <- lockfile$Packages$renv
  new <- renv_upgrade_find_record(version)

  # check for some form of change
  if (renv_records_equal(old, new)) {
    fmt <- "- renv [%s] is already installed and active for this project."
    writef(fmt, renv_metadata_version_friendly())
    return(FALSE)
  }

  if (prompt || renv_verbose()) {
    renv_pretty_print_records_pair(
      "A new version of the renv package will be installed:",
      list(renv = old),
      list(renv = new),
      "This project will use the newly-installed version of renv."
    )
  }

  cancel_if(prompt && !proceed())

  renv_scope_restore(
    project   = project,
    library   = renv_libpaths_active(),
    records   = list(renv = new),
    packages  = "renv",
    recursive = FALSE
  )

  # retrieve and install renv
  records <- retrieve("renv")
  renv_install_impl(records)

  # update the lockfile
  lockfile <- renv_lockfile_load(project = project)
  records <- renv_lockfile_records(lockfile) %||% list()
  records$renv <- new
  renv_lockfile_records(lockfile) <- records
  renv_lockfile_save(lockfile, project = project)

  # now update the infrastructure to use this version of renv.
  # do this in a separate process to avoid issues that could arise
  # if the old version of renv is still loaded
  #
  # https://github.com/rstudio/renv/issues/1546
  writef("- Updating activate script")
  code <- substitute({
    renv <- asNamespace("renv"); renv$summon()
    version <- renv_metadata_version_create(record)
    renv_infrastructure_write(project, version = version)
  }, list(project = project, record = records[["renv"]]))

  script <- renv_scope_tempfile("renv-activate-", fileext = ".R")
  writeLines(deparse(code), con = script)

  args <- c("--vanilla", "-s", "-f", renv_shell_path(script))
  r(args, stdout = FALSE, stderr = FALSE)

  if (reload) {
    renv_restart_request(project)
  }

  invisible(TRUE)

}

renv_upgrade_find_record <- function(version) {

  if (is.null(version))
    renv_upgrade_find_record_default()
  else
    renv_upgrade_find_record_dev(version)

}

renv_upgrade_find_record_default <- function() {

  # check if the package is available on R repositories.
  # if not, prefer GitHub
  record <- catch(renv_available_packages_latest("renv"))
  if (inherits(record, "error"))
    return(renv_upgrade_find_record_dev())

  # check the version reported by R repositories.
  # if it's older than current renv, then prefer GitHub
  version <- record$Version
  if (package_version(version) < renv_package_version("renv"))
    return(renv_upgrade_find_record_dev())

  # ok -- install from repository
  record

}

renv_upgrade_find_record_dev <- function(version = NULL) {
  version <- version %||% renv_upgrade_find_record_dev_latest()
  entry <- paste("rstudio/renv", version, sep = "@")
  renv_remotes_resolve(entry)
}


renv_upgrade_find_record_dev_latest <- function() {

  # download tags
  url <- "https://api.github.com/repos/rstudio/renv/tags"
  destfile <- tempfile("renv-tags-", fileext = ".json")
  download(url, destfile = destfile, quiet = TRUE)
  json <- renv_json_read(destfile)

  # find latest version
  names <- extract_chr(json, "name")
  versions <- numeric_version(names, strict = FALSE)
  latest <- sort(versions, decreasing = TRUE)[[1]]
  names[versions %in% latest][[1L]]

}

renv_upgrade_reload <- function() {

  # we need to remove the task callbacks here, as otherwise
  # we'll run into trouble trying to remove task callbacks
  # within a task callback
  renv_task_unload()

  # now define and add a callback to reload renv; use the base namespace
  # to avoid carrying along any bits of the current renv environment
  callback <- function(...) {
    unloadNamespace("renv")
    loadNamespace("renv")
    invisible(FALSE)
  }

  environment(callback) <- baseenv()

  # add the task callback; don't name it so that the renv infrastructure
  # doesn't try to remove this callback (it'll resolve and remove itself)
  addTaskCallback(callback)

  invisible(TRUE)

}
