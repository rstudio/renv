
#' Toggle between Global and Project-local Virtual Environments
#'
#' Toggle between global and project-local virtual environments.
#'
#' This function is for expert use only -- the typical way to initialize a
#' project-local environment is with the [init()] function.
#'
#' Setting this toggles how the various other `renv` APIs work -- for example,
#' in the following:
#'
#' ```
#' renv::local(TRUE)
#' renv::create("test")
#' renv::activate("test")
#' ```
#'
#' a project-local virtual environment called `"test"` is created, and then
#' activated and used for that project.
#'
#' @param local Boolean; should project-local virtual environments be used?
#'
#' @export
local <- function(local) {

  if (missing(local))
    return(renv_state$local())

  local <- as.logical(local)
  renv_state$local(local)
  fmt <- "* Using %s virtual environments."
  vmessagef(fmt, if (local) "project-local" else "global")
  invisible(local)

}
