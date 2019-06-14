
#' Isolate a Project
#'
#' Copy packages from the `renv` cache directly into the project library, so
#' that the project can continue to function independently of the `renv` cache.
#'
#' @inheritParams renv-params
#' @export
isolate <- function(project = NULL) {
  project <- project %||% renv_project()
  settings$use.cache(FALSE)
}
