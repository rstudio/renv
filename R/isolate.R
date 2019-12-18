
#' Isolate a Project
#'
#' Copy packages from the `renv` cache directly into the project library, so
#' that the project can continue to function independently of the `renv` cache.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # isolate a project
#' renv::isolate()
#'
#' }
isolate <- function(project = NULL) {
  project <- renv_project_resolve(project)
  settings$use.cache(FALSE)
  invisible(project)
}
