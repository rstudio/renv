
#' Run a script
#'
#' Run an \R script, in the context of a project using renv. The script will
#' be run within an \R sub-process.
#'
#' @inherit renv-params
#'
#' @param script The path to an \R script.
#'
#' @param job Run the requested script as an RStudio job? Requires a recent
#'   version of both RStudio and the rstudioapi packages. When `NULL`, the
#'   script will be run as a job if possible, and as a regular \R process
#'   launched by [system2()] if not.
#'
#' @param name The name to associate with the job, for scripts run as a job.
#'
#' @param project The path to the renv project. This project will be loaded
#'   before the requested script is executed. When `NULL` (the default), renv
#'   will automatically determine the project root for the associated script
#'   if possible.
#'
#' @export
run <- function(script, ..., job = NULL, name = NULL, project = NULL) {

  renv_scope_error_handler()
  renv_dots_check(...)

  script <- renv_path_normalize(script, mustWork = TRUE)

  # find the project directory
  project <- project %||% renv_file_find(script, function(path) {
    paths <- file.path(path, c("renv", "renv.lock"))
    if (any(file.exists(paths)))
      return(path)
  })

  if (is.null(project)) {
    fmt <- "could not determine project root for script '%s'"
    stopf(fmt, renv_path_aliased(script))
  }

  # ensure that it has an activate script
  activate <- renv_paths_activate(project = project)
  if (!file.exists(activate)) {
    fmt <- "project '%s' does not have an renv activate script"
    stopf(fmt, renv_path_aliased(project))
  }

  # run as a job when possible in RStudio
  jobbable <-
    !identical(job, FALSE) &&
    renv_rstudio_available() &&
    renv_package_installed("rstudioapi") &&
    renv_package_version("rstudioapi") >= "0.10" &&
    rstudioapi::verifyAvailable("1.2.1335")

  if (identical(job, TRUE) && identical(jobbable, FALSE))
    stopf("cannot run script as job: required versions of RStudio + rstudioapi not available")

  if (jobbable)
    renv_run_job(script = script, name = name, project = project)
  else
    renv_run_impl(script = script, name = name, project = project)

}

renv_run_job <- function(script, name, project) {

  activate <- renv_paths_activate(project = project)
  exprs <- expr({
    source(!!activate)
    source(!!script)
  })

  code <- deparse(exprs)
  jobscript <- tempfile("renv-job-", fileext = ".R")
  writeLines(code, con = jobscript)

  rstudioapi::jobRunScript(
    path       = jobscript,
    workingDir = project,
    name       = name
  )

}

renv_run_impl <- function(script, name, project) {
  renv_scope_wd(project)
  system2(R(), c("-s", "-f", renv_shell_path(script)))
}
