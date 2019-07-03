
#' Upgrade renv
#'
#' Upgrade the version of `renv` associated with a project.
#'
#' This function will attempt to install `renv` from two locations:
#'
#' 1. The active CRAN repositories (as specified in `getOption("repos")`);
#' 2. From the `renv` project's [GitHub page](https://github.com/rstudio/renv).
#'
#' When `version = NULL` (the default), only the active CRAN repositories will
#' be consulted. Hence, this can be used to upgrade a project to the latest
#' released version of `renv` on CRAN.
#'
#' If you'd instead like to try out a development version of `renv`, you can
#' explicitly request a different version of `renv` and that version of the
#' package will be downloaded and installed from GitHub. Use
#' `version = "master"` to install the latest development version of `renv`.
#'
#' @inheritParams renv-params
#'
#' @param version The version of `renv` to be installed. By default, the
#'   latest version of `renv` available on CRAN is used.
#'
#' @param confirm Boolean; confirm upgrade before proceeding?
#'
#' @export
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
    writef(fmt, current)
    return(TRUE)
  }

  renv_pretty_print(
    sprintf("[%s] -> [%s]", current, request),
    "A new version of the renv package will be installed:",
    "This project will use the newly-installed version of renv."
  )

  if (confirm && !proceed()) {
    writeLines("Operation aborted.")
    return(FALSE)
  }

  renv_restore_begin(records = records, packages = "renv", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  # retrieve renv
  records <- renv_retrieve("renv")
  record <- records[[1]]

  # set library paths temporarily to install into bootstrap library
  library <- renv_paths_bootstrap("renv", record$Version)
  ensure_directory(library)
  renv_scope_libpaths(library)
  renv_install_impl(record, project)

  # upgrade the lockfile
  lockfile <- renv_lockfile_load(project = project)
  lockfile$renv$Version <- record$Version
  renv_lockfile_save(lockfile, project = project)

  # now update the infrastructure to use this version of renv
  renv_infrastructure_write(project, version = record$Version)

  # and restart
  renv_request_restart(project, reason = "renv updated")

}

renv_upgrade_find_record <- function(version) {

  # first, try to find the requested version on CRAN
  for (type in renv_package_pkgtypes()) {
    entry <- catch(renv_available_packages_entry("renv", type, version))
    if (!inherits(entry, "error")) {
      record <- c(entry[c("Package", "Version", "Repository")], Type = "source")
      return(record)
    }
  }

  # when version is NULL, missing renv is an error
  if (is.null(version))
    stopf("failed to find package renv in the active CRAN repositories")

  # no luck, try using a GitHub remote instead
  entry <- paste("rstudio/renv", version %||% "master", sep = "@")
  renv_remotes_parse(entry)

}
