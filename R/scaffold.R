#' Generate project infrastructure
#'
#' @description
#' Create the renv project infrastructure. This will:
#'
#' - Create a project library, `renv/library`.
#'
#' - Install renv into the project library.
#'
#' - Update the project `.Rprofile` to call `source("renv/activate.R")` so
#'   that renv is automatically loaded for new \R sessions launched in
#'   this project.
#'
#' - Create `renv/.gitignore`, which tells git to ignore the project library.
#'
#' - Create `.Rbuildignore`, if the project is also a package. This tells
#'   `R CMD build` to ignore the renv infrastructure,
#'
#' - Write a (bare) [lockfile], `renv.lock`.
#'
#' @inheritParams renv-params
#'
#' @param version The version of renv to associate with this project. By
#'   default, the version of renv currently installed is used.
#'
#' @param repos The \R repositories to associate with this project.
#'
#' @param settings A list of renv settings, to be applied to the project
#'   after creation. These should map setting names to the desired values.
#'   See [settings] for more details.
#'
#' @examples
#'
#' \dontrun{
#' # create scaffolding with 'devtools' ignored
#' renv::scaffold(settings = list(ignored.packages = "devtools"))
#' }
#'
#' @export
scaffold <- function(project  = NULL,
                     version  = NULL,
                     repos    = getOption("repos"),
                     settings = NULL)
{
  renv_scope_error_handler()
  renv_scope_options(repos = repos)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  # install renv into project library
  renv_imbue_impl(project, version)

  # write out project infrastructure
  renv_infrastructure_write(project, version)

  # update project settings
  if (is.list(settings))
    renv_settings_persist(project, settings)

  # generate a lockfile
  lockfile <- renv_lockfile_create(
    project  = project,
    libpaths = renv_paths_library(project = project),
    type     = "implicit"
  )

  renv_lockfile_write(lockfile, file = renv_lockfile_path(project))

  # notify user
  fmt <- "- renv infrastructure has been generated for project %s."
  writef(fmt, renv_path_pretty(project))

  # return project invisibly
  invisible(project)
}
