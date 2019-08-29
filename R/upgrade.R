
#' Upgrade renv
#'
#' Upgrade the version of `renv` associated with a project.
#'
#' By default, this function will attempt to install the latest version of
#' `renv` as available on the active CRAN repositories. If you'd instead like to
#' try out a development version of `renv`, you can explicitly request a
#' different version of `renv` and that version of the package will be
#' downloaded and installed from GitHub. Use `version = "master"` to install the
#' latest development version of `renv`, as from the `renv` project's [GitHub
#' page](https://github.com/rstudio/renv).
#'
#' @inheritParams renv-params
#'
#' @param version The version of `renv` to be installed. By default, the latest
#'   version of `renv` as available on the active CRAN repositories is used.
#'
#' @param confirm Boolean; confirm upgrade before proceeding?
#'
#' @export
#'
#' @examples
#' \donttest{
#' \dontrun{
#'
#' # upgrade to the latest version of renv on CRAN
#' renv::upgrade()
#'
#' # upgrade to the latest version of renv on GitHub (development version)
#' renv::upgrade(version = "master")
#'
#' }
#' }
upgrade <- function(project = NULL,
                    version = NULL,
                    confirm = interactive())
{
  renv_scope_error_handler()
  invisible(renv_upgrade_impl(project, version, confirm))
}

renv_upgrade_impl <- function(project, version, confirm) {

  project <- project %||% renv_project()

  # conduct a mini-restore for renv itself
  record <- renv_upgrade_find_record(version)
  records <- list(renv = record)

  # produce nice messages based on package versions
  current <- renv_activate_version(project)
  request <- record$Version
  if (version_compare(current, request) == 0) {
    fmt <- "renv [%s] is already installed and active for this project."
    vwritef(fmt, current)
    return(TRUE)
  }

  if (confirm || renv_verbose()) {
    renv_pretty_print(
      sprintf("[%s] -> [%s]", current, request),
      "A new version of the renv package will be installed:",
      "This project will use the newly-installed version of renv."
    )
  }

  if (confirm && !proceed()) {
    writeLines("Operation aborted.")
    return(FALSE)
  }

  renv_restore_begin(records = records, packages = "renv", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  # retrieve renv
  records <- renv_retrieve("renv")
  record <- records[[1]]

  # install renv
  renv_install_impl(record, project)

  # upgrade the lockfile
  lockfile <- renv_lockfile_load(project = project)
  lockfile$renv$Version <- record$Version
  renv_lockfile_save(lockfile, project = project)

  # now update the infrastructure to use this version of renv
  renv_infrastructure_write(project, version = record$Version)

  # and restart
  renv_request_restart(project, reason = "renv upgraded")

}

renv_upgrade_find_record <- function(version) {

  # when version is NULL, attempt to get latest version of renv on CRAN;
  # otherwise, fall back to the latest version of GitHub
  if (is.null(version)) {
    record <- catch(renv_records_cran_latest("renv"))
    if (!inherits(record, "error"))
      return(record)
  }

  # otherwise, attempt to install the latest version of renv from GitHub
  entry <- paste("rstudio/renv", version %||% "master", sep = "@")
  renv_remotes_resolve(entry)

}
