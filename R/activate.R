
#' Activate a Project
#'
#' Activate a project, thereby loading it in the current session and also
#' writing the infrastructure necessary to ensure the project is auto-loaded
#' for newly-launched \R sessions.
#'
#' Using `activate()` will:
#'
#' 1. Load the requested project via [renv::load()],
#'
#' 2. Add `source("renv/activate.R")` to the project `.Rprofile`, thereby
#'    instructing newly-launched \R sessions to automatically load the
#'    current project.
#'
#' Normally, `activate()` is called as part of [renv::init()] when a project
#' is first initialized. However, `activate()` can be used to activate
#' (or re-activate) an `renv` project -- for example, if the project was shared
#' without the auto-loader included in the project `.Rprofile`, or because
#' that project was previously deactivated (via [renv::deactivate()]).
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
activate <- function(project = NULL, profile = NULL) {

  renv_consent_check()
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  renv_profile_set(profile)

  renv_activate_impl(
    project = project,
    profile = profile,
    version = NULL,
    restart = FALSE,
    quiet   = FALSE
  )

  invisible(project)

}

renv_activate_impl <- function(project,
                               profile,
                               version,
                               restart,
                               quiet)
{
  # prepare renv infrastructure
  renv_infrastructure_write(
    project = project,
    profile = profile,
    version = version
  )

  # try to load the project
  load(project, quiet = quiet)

  # ensure renv is imbued into the new library path if necessary
  if (!renv_tests_running())
    renv_imbue_self(project)

  # restart session if requested
  if (restart)
    renv_restart_request(project, reason = "renv activated")
  else if (renv_rstudio_available())
    renv_rstudio_initialize(project)

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

  fmt <- "failed to determine renv version for project %s"
  stopf(fmt, renv_path_pretty(project))

}

renv_activate_version_activate <- function(project) {

  activate <- renv_paths_activate(project = project)
  if (!file.exists(activate))
    return(NULL)

  contents <- readLines(activate, warn = FALSE)
  line <- grep("^\\s*version", contents, value = TRUE)
  parsed <- parse(text = line)[[1L]]
  parsed[[3L]]

}

renv_activate_version_lockfile <- function(project) {

  path <- renv_lockfile_path(project)
  if (!file.exists(path))
    return(NULL)

  lockfile <- renv_lockfile_read(path)
  lockfile$Packages[["renv"]]$Version %||% lockfile$renv$Version

}

renv_activate_version_default <- function(project) {
  renv_metadata_version()
}

renv_activate_prompt <- function(action, library, prompt, project) {

  # check whether we should ask user to activate
  ask <-
    config$activate.prompt() &&
    prompt &&
    interactive() &&
    is.null(library) &&
    !identical(project, Sys.getenv("RENV_PROJECT"))

  if (!ask)
    return(FALSE)

  fmt <- lines(
    "",
    "This project has not yet been activated.",
    "Activating this project will ensure the project library is used when %s() is called.",
    "Please see `?renv::activate` for more details.",
    ""
  )

  notice <- sprintf(fmt, action)
  vwritef(notice)

  fmt <- "Would you like to activate this project before %s() is called?"
  question <- sprintf(fmt, action)
  response <- ask(question, default = TRUE)
  if (!response)
    return(FALSE)

  activate(project = project)
  return(TRUE)

}
