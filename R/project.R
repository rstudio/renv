
#' Retrieve the Active Project
#'
#' Retrieve the path to the active project (if any).
#'
#' @param default The value to return when no project is
#'   currently active. Defaults to `NULL`.
#'
#' @export
#'
#' @examples
#' \donttest{
#' \dontrun{
#'
#' # get the currently-active renv project
#' renv::project()
#'
#' }
#' }
project <- function(default = NULL) {
  renv_project(default = default)
}

renv_project <- function(default = getwd()) {
  project <- Sys.getenv("RENV_PROJECT", unset = NA)
  if (is.na(project))
    return(default)
  project
}

renv_project_initialized <- function(project) {

  lockfile <- renv_lockfile_path(project)
  if (file.exists(lockfile))
    return(TRUE)

  library <- renv_paths_library(project = project)
  if (file.exists(library))
    return(TRUE)

  FALSE

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

renv_project_records <- function(project) {

  # if this project has a DESCRIPTION file, use it to provide records
  descpath <- file.path(project, "DESCRIPTION")
  if (file.exists(descpath))
    return(renv_project_records_description(descpath))

}

renv_project_records_description <- function(descpath) {

  # parse explicit dependencies from DESCRIPTION file
  # TODO: handle version requirements
  deps <- renv_dependencies_discover_description(descpath)
  records <- lapply(deps$Package, renv_remotes_resolve)
  names(records) <- extract_chr(records, "Package")

  # parse remotes (if any)
  desc <- renv_description_read(descpath)
  remotes <- desc$Remotes
  if (!is.null(remotes)) {
    splat <- strsplit(remotes, "\\s*,\\s*")[[1]]
    resolved <- lapply(splat, renv_remotes_resolve)
    names(resolved) <- extract_chr(resolved, "Package")
    records[names(resolved)] <- resolved
  }

  # return records
  records

}
