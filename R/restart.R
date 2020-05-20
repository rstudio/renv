
renv_request_restart <- function(project, reason, ...) {

  # if we're running in RStudio, explicitly open the project
  # if it differs from the current project
  if (identical(.Platform$GUI, "RStudio")) {
    status <- renv_request_restart_rstudio(project, reason, ...)
    return(invisible(status))
  }

  renv_request_restart_default(project, reason, ...)

}

renv_request_restart_default <- function(project, reason, ...) {

  # use 'restart' helper defined by front-end (if any)
  restart <- getOption("restart")
  if (is.function(restart))
    return(invisible(restart()))

  # otherwise, ask the user to restart
  if (interactive()) {
    fmt <- "* %s -- please restart the R session."
    vwritef(fmt, sprintf(reason, ...), con = stderr())
  }

}

renv_request_restart_rstudio <- function(project, reason, ...) {

  # if we're running tests, don't restart
  if (renv_testing())
    return(renv_request_restart_default(project, reason, ...))

  # if we don't have a tools env, bail
  tools <- catch(as.environment("tools:rstudio"))
  if (inherits(tools, "error"))
    return(renv_request_restart_default(project, reason, ...))

  # if RStudio is too old, use default restart impl
  old <-
    is.null(tools$.rs.getProjectDirectory) ||
    is.null(tools$.rs.api.openProject)

  if (old)
    return(renv_request_restart_default(project, reason, ...))

  # if the requested project matches the current project, just
  # restart the R session
  projdir <- tools$.rs.getProjectDirectory() %||% ""
  if (renv_file_same(projdir, project)) {
    restart <- getOption("restart")
    if (is.function(restart))
      return(restart())
  }

  # otherwise, explicitly open the new project
  tools$.rs.api.openProject(project, newSession = FALSE)

}
