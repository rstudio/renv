
renv_restart_request <- function(project = NULL, reason = "", ...) {

  project <- renv_project_resolve(project)

  # if we're running in RStudio, explicitly open the project
  # if it differs from the current project
  if (renv_rstudio_available()) {
    status <- renv_restart_request_rstudio(project, reason, ...)
    return(invisible(status))
  }

  renv_restart_request_default(project, reason, ...)

}

renv_restart_request_default <- function(project, reason, ...) {

  # use 'restart' helper defined by front-end (if any)
  restart <- getOption("restart")
  if (is.function(restart))
    return(invisible(restart()))

  # otherwise, ask the user to restart
  if (interactive()) {
    fmt <- "- %s -- please restart the R session."
    writef(fmt, sprintf(reason, ...))
  }

}

renv_restart_request_rstudio <- function(project, reason, ...) {

  # if we're running tests, don't restart
  if (renv_tests_running())
    return(renv_restart_request_default(project, reason, ...))

  # if we don't have a tools env, bail
  tools <- catch(as.environment("tools:rstudio"))
  if (inherits(tools, "error"))
    return(renv_restart_request_default(project, reason, ...))

  # if RStudio is too old, use default restart impl
  old <-
    is.null(tools$.rs.getProjectDirectory) ||
    is.null(tools$.rs.api.openProject)

  if (old)
    return(renv_restart_request_default(project, reason, ...))

  # if the requested project matches the current project, just
  # restart the R session -- but note that we cannot respect
  # the 'restart' option here as the version RStudio uses
  # tries to preserve session state that we need to change.
  #
  # https://github.com/rstudio/renv/issues/1530
  projdir <- tools$.rs.getProjectDirectory() %||% ""
  if (renv_file_same(projdir, project)) {
    restart <- getOption("renv.restart.function", default = function() {
      tools$.rs.api.executeCommand("restartR", quiet = TRUE)
    })
    return(invisible(restart()))
  }

  # otherwise, explicitly open the new project
  invisible(tools$.rs.api.openProject(project, newSession = FALSE))

}
