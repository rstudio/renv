
#' Retrieve the Active Project
#'
#' Retrieve the path to the active project (if any).
#'
#' @param default The value to return when no project is
#'   currently active. Defaults to `NULL`.
#'
#' @export
project <- function(default = NULL) {
  renv_project(default = default)
}

renv_project <- function(default = getwd()) {
  project <- Sys.getenv("RENV_PROJECT", unset = NA)
  if (is.na(project))
    return(default)
  project
}

renv_project_type <- function(path) {

  # check for R package projects
  descpath <- file.path(path, "DESCRIPTION")
  if (file.exists(descpath)) {

    desc <- renv_description_read(descpath)

    # check for explicitly recorded type
    type <- desc$Type
    if (!is.null(type))
      return(tolower(type))

    # infer otherwise from 'Package' field otherwise
    package <- desc$Package
    if (!is.null(package))
      return("package")

  }

  "unknown"

}
