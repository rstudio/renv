
#' Activate or deactivate a project
#'
#' @description
#' `activate()` enables renv for a project in both the current session and
#' in all future sessions. You should not generally need to call `activate()`
#' yourself as it's called automatically by [renv::init()], which is the best
#' way to start using renv in a new project.
#'
#' `activate()` first calls [renv::scaffold()] to set up the project
#' infrastructure. Most importantly, this creates a project library and adds a
#' `.Rprofile` to ensure that the project library is automatically used for all
#' future instances of the project. It then calls [renv::load()] to use the
#' project library for the current session.
#'
#' `deactivate()` removes the infrastructure automatically activate renv in
#' new session. By default it will remove the auto-loader from the `.Rprofile`;
#' use `clean = TRUE` to also delete the lockfile and the project library.
#'
#' @inherit renv-params
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
#' # deactivate the currently-activated project
#' renv::deactivate()
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
  lockfile$Packages[["renv"]]$RemoteSha %||%
    lockfile$Packages[["renv"]]$Version %||%
    lockfile[["renv"]]$Version

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
    !renv_project_loaded(project) &&
    !is_testing()

  # for snapshot, since users might want to snapshot their system library
  # in an renv-lite configuration, only prompt if it looks like they're
  # working within an renv project that hasn't been loaded
  if ("snapshot" %in% action) {
    libpath <- renv_paths_library(project = project)
    ask <- ask && file.exists(libpath)
  }

  if (!ask)
    return(FALSE)

  renv_activate_prompt_impl(action, project)


}

renv_activate_prompt_impl <- function(action, project = NULL) {
  title <- c(
    sprintf("It looks like you've called renv::%s() in a non-renv project.", action),
    "How would you like to proceed?"
  )
  choices <- c(
    activate = "Activate the project and use the project library.",
    continue = "Do not activate the project and use the current library paths.",
    cancel = "Cancel and resolve the problem another way."
  )

  choice <- menu(choices, title, default = 2)
  switch(choice,
    activate = {activate(project = project); TRUE},
    continue = FALSE,
    cancel = cancel(),
  )
}
