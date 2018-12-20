
#' Initialize a Project-local Virtual Environment
#'
#' Discover packages used within the current project, and then initialize
#' a virtual environment with those packages.
#'
#' @param project The project directory.
#' @param ... Optional arguments passed to [create()].
#'
#' @export
init <- function(project = NULL, ...) {
  project <- project %||% getwd()

  # switch to local mode
  local <- renv_local()
  renv_set_local(TRUE)
  on.exit(renv_set_local(local), add = TRUE)

  # create the virtual environment
  name <- basename(project)
  create(name = name, ...)

  # find packages used in this project
  if (renv_verbose())
    message("Discovering package dependencies ... ", appendLF = FALSE)

  deps <- discover_dependencies(project)

  if (renv_verbose())
    message("Done!")

  # keep only non-base packages
  all <- unique(deps$Package)
  base <- installed.packages(lib.loc = .Library, priority = "base")
  packages <- setdiff(all, rownames(base))

  # link these packages into the private library
  lapply(packages, function(package) {
    renv_snapshot_description(package)
  })



}
