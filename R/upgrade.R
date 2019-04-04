
#' Upgrade renv
#'
#' Upgrade the version of `renv` associated with a project.
#'
#' @inheritParams renv-params
#'
#' @param remote A 'remote' specification, as understood by the
#'   [remotes](https://cran.r-project.org/package=remotes) package. (Note that
#'   not all remote specifications are yet supported by `renv`.) See
#'   <https://cran.r-project.org/web/packages/remotes/vignettes/dependencies.html>
#'    for more details. When `NULL` (the default), the latest version of `renv`
#'   available on GitHub is retrieved instead.
#'
#' @param confirm Boolean; confirm upgrade before proceeding?
#'
#' @export
upgrade <- function(project = NULL,
                    remote = "rstudio/renv",
                    confirm = interactive())
{
  invisible(renv_upgrade_impl(project, remote, confirm))
}

renv_upgrade_impl <- function(project, remote, confirm) {

  project <- project %||% renv_project()

  # conduct a mini-restore for renv itself
  record <- renv_remotes_parse(remote)
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
    sprintf("[%s] -> [%s]", renv_package_version("renv"), record$Version),
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
  records <- renv_retrieve("renv", records)
  record <- records[[1]]

  # set library paths temporarily to install into bootstrap library
  library <- renv_paths_bootstrap("renv", record$Version)
  ensure_directory(library)
  renv_scope_libpaths(library)
  renv_install_impl(record, library)

  # now update the infrastructure to use this version of renv
  renv_write_infrastructure(project, version = record$Version)

  # and restart
  renv_request_restart(project, reason = "renv updated")

}
