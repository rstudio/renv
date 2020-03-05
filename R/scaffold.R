#' Generate renv Project Infrastructure
#'
#' Write the `renv` project infrastructure for a project.
#'
#' Invoking `renv::scaffold()` will:
#'
#' - Install `renv` into the project library,
#'
#' - Update the project `.Rprofile` so that `renv` is automatically loaded
#'   for new \R sessions launched in this project, and
#'
#' - Write a bare lockfile `renv.lock`.
#'
#' @inheritParams renv-params
#'
#' @param version The version of `renv` to associate with this project. By
#'   default, the version of `renv` currently installed is used.
#'
#' @param repos The \R repositories to associate with this project.
#'
#' @export
scaffold <- function(project = NULL,
                     version = NULL,
                     repos   = getOption("repos"))
{
  renv_scope_error_handler()
  renv_scope_options(repos = repos)

  project <- renv_project_resolve(project)

  # install renv into project library
  renv <- renv_imbue_impl(project, version)

  # write out project infrastructure
  renv_infrastructure_write(project, version)

  # generate a lockfile
  lockfile <- renv_lockfile_create(
    project = project,
    library = renv_paths_library(project = project),
    type    = "implicit"
  )

  renv_lockfile_write(lockfile, file = renv_lockfile_path(project))

  # notify user
  fmt <- "* renv infrastructure has been generated for project %s."
  vwritef(fmt, renv_path_pretty(project))

  # return project invisibly
  invisible(project)
}
