
#' Activate a Project
#'
#' Use `activate()` to write the infrastructure needed to ensure that
#' newly-launched \R projects will load the project's private library on launch,
#' alongside any other project-specific state recorded for the project.
#'
#' @inherit renv-params
#'
#' @family renv
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # activate the current project
#' renv::activate()
#'
#' # activate a separate project
#' renv::activate("~/projects/analysis")
#'
#' }
activate <- function(project = NULL) {
  renv_consent_check()
  renv_scope_error_handler()
  project <- renv_project_resolve(project)
  renv_activate_impl(project, NULL, FALSE, FALSE)
  invisible(project)
}

renv_activate_impl <- function(project, version, restart, quiet) {

  # prepare renv infrastructure
  renv_infrastructure_write(project, version)

  # try to load the project
  load(project, quiet = quiet)

  # ensure renv is installed
  if (!renv_testing())
    renv_imbue_self(project = project)

  # restart session
  if (restart)
    renv_request_restart(project, reason = "renv activated")

}

renv_activate_version <- function(project) {

  # try to get version from activate.R
  methods <- list(
    renv_activate_version_lockfile,
    renv_activate_version_activate,
    renv_activate_version_default
  )

  for (method in methods) {
    version <- catch(method(project))
    if (is.character(version))
      return(version)
  }

  fmt <- "failed to determine renv version for project '%s'"
  stopf(fmt, aliased_path(project))

}

renv_activate_version_activate <- function(project) {

  activate <- file.path(project, "renv/activate.R")
  if (!file.exists(activate))
    return(NULL)

  contents <- readLines(activate, warn = FALSE)
  line <- grep("^\\s*version", contents, value = TRUE)
  parsed <- parse(text = line)[[1]]
  parsed[[3]]

}

renv_activate_version_lockfile <- function(project) {

  path <- renv_lockfile_path(project)
  if (!file.exists(path))
    return(NULL)

  lockfile <- renv_lockfile_read(path)
  lockfile$Packages[["renv"]]$Version %||% lockfile$renv$Version

}

renv_activate_version_default <- function(project) {
  renv_namespace_version("renv")
}
