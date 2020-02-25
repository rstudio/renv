
#' Upgrade renv
#'
#' Upgrade the version of `renv` associated with a project.
#'
#' By default, this function will attempt to install the latest version of
#' `renv` as available on the active R package repositories. If you'd instead
#' like to try out a development version of `renv`, you can explicitly request a
#' different version of `renv` and that version of the package will be
#' downloaded and installed from GitHub. Use `version = "master"` to install the
#' latest development version of `renv`, as from the `renv` project's [GitHub
#' page](https://github.com/rstudio/renv).
#'
#' @inherit renv-params
#'
#' @param version The version of `renv` to be installed. By default, the latest
#'   version of `renv` as available on the active R package repositories is
#'   used.
#'
#' @param prompt Boolean; prompt upgrade before proceeding?
#'
#' @param reload Boolean; reload `renv` after install? When `NULL` (the
#'   default), `renv` will be re-loaded only if updating `renv` for the
#'   active project. Note that this may fail if you've loaded packages
#'   which also depend on `renv`.
#'
#' @return A boolean value, indicating whether the requested version of
#'   `renv` was successfully installed. Note that this function is normally
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
#' renv::upgrade(version = "master")
#'
#' }
upgrade <- function(project = NULL,
                    version = NULL,
                    reload  = NULL,
                    prompt = interactive())
{
  renv_scope_error_handler()
  invisible(renv_upgrade_impl(project, version, reload, prompt))
}

renv_upgrade_impl <- function(project, version, reload, prompt) {

  project <- renv_project_resolve(project)
  reload <- reload %||% identical(project, renv_project())

  old <- renv_snapshot_description(package = "renv")
  new <- renv_upgrade_find_record(version)

  # check for some form of change
  if (renv_records_equal(old, new)) {
    fmt <- "* renv [%s] is already installed and active for this project."
    vwritef(fmt, new$Version)
    return(TRUE)
  }

  if (prompt || renv_verbose()) {
    renv_pretty_print_records_pair(
      list(renv = old), list(renv = new),
      "A new version of the renv package will be installed:",
      "This project will use the newly-installed version of renv."
    )
  }

  if (prompt && !proceed()) {
    writeLines("Operation aborted.")
    return(FALSE)
  }

  renv_scope_restore(
    project = project,
    records = list(renv = new),
    packages = "renv",
    recursive = FALSE
  )

  # retrieve renv
  records <- renv_retrieve("renv")
  record <- records[[1]]

  # install renv
  renv_install_impl(record)

  # update the lockfile
  lockfile <- renv_lockfile_load(project = project)
  records <- renv_records(lockfile) %||% list()
  records$renv <- new
  renv_records(lockfile) <- records
  renv_lockfile_save(lockfile, project = project)

  # now update the infrastructure to use this version of renv
  renv_infrastructure_write(project, version = record$Version)

  # reload renv
  if (reload)
    renv_package_reload("renv")

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
  if (package_version(version) < renv_namespace_version("renv"))
    return(renv_upgrade_find_record_dev())

  # ok -- install from repository
  record

}

renv_upgrade_find_record_dev <- function(version = NULL) {
  entry <- paste("rstudio/renv", version %||% "master", sep = "@")
  renv_remotes_resolve(entry)
}
