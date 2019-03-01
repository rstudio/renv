
#' Retrieve the Active Project
#'
#' Retrieve the path to the active project (if any).
#'
#' @export
project <- function() {
  renv_state_impl_get("project", NULL)
}
