
#' Retrieve the Active Project
#'
#' Retrieve the path to the active project (if any).
#'
#' @param default The value to return when no project is
#'   currently active. Defaults to `NULL`.
#'
#' @export
#'
#' @return The active project directory, as a length-one character vector.
#'
#' @examples
#' \dontrun{
#'
#' # get the currently-active renv project
#' renv::project()
#'
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
  if (!file.exists(descpath))
    return("unknown")

  desc <- catch(renv_description_read(descpath))
  if (inherits(desc, "error")) {
    warning(desc)
    return("unknown")
  }

  # check for explicitly recorded type
  type <- desc$Type
  if (!is.null(type))
    return(tolower(type))

  # infer otherwise from 'Package' field otherwise
  package <- desc$Package
  if (!is.null(package))
    return("package")

}

renv_project_records <- function(project) {

  # if this project has a DESCRIPTION file, use it to provide records
  descpath <- file.path(project, "DESCRIPTION")
  if (file.exists(descpath))
    return(renv_project_records_description(project, descpath))

}

renv_project_records_description <- function(project, descpath) {

  # first, parse remotes (if any)
  remotes <- renv_project_records_description_remotes(project, descpath)

  # next, find packages mentioned in the DESCRIPTION file
  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")
  deps <- renv_dependencies_discover_description(descpath, fields)
  ignored <- settings$ignored.packages(project = project)
  packages <- setdiff(deps$Package, c("R", ignored))

  # if any Roxygen fields are included,
  # infer a dependency on roxygen2 and devtools
  desc <- renv_description_read(descpath)
  if (any(grepl("^Roxygen", names(desc))))
    packages <- union(packages, c("devtools", "roxygen2"))

  # now, try to resolve the packages
  records <- lapply(packages, function(package) {
    remotes[[package]] %||% renv_remotes_resolve(package)
  })
  names(records) <- extract_chr(records, "Package")

  # return records
  records

}

renv_project_records_description_remotes <- function(project, descpath) {

  desc <- renv_description_read(descpath)

  remotes <- desc$Remotes
  if (is.null(desc$Remotes))
    return(list())

  splat <- strsplit(remotes, "\\s*,\\s*")[[1]]
  resolved <- lapply(splat, renv_remotes_resolve)
  names(resolved) <- extract_chr(resolved, "Package")
  resolved

}
