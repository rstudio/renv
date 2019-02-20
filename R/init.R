
#' Initialize a Project
#'
#' Discover packages used within the current project, and then initialize a
#' project-local private \R library with those packages. The currently-installed
#' versions of any packages in use (as detected within the default R libraries)
#' are then installed to the project's private library.
#'
#' The primary steps taken when initializing a new virtual environment are:
#'
#' 1. \R package dependencies are discovered within the \R files used within
#'    the project with [discover_dependencies()];
#'
#' 2. Discovered packages are copied into the `renv` global package cache, so
#'    these packages can be re-used across future projects as necessary;
#'
#' 3. Any missing \R package dependencies discovered are then installed into
#'    the project's private library;
#'
#' 4. A lockfile capturing the state of the project's library is created
#'    with [snapshot()];
#'
#' 5. The project is activated with [activate()].
#'
#' This mimics the workflow provided by `packrat::init()`, but with more
#' reasonable default behaviors -- in particular, `renv` does not attempt
#' to download and store package sources, and `renv` will re-use packages
#' that have already been installed whenever possible.
#'
#' @param project The project directory.
#' @param force Boolean; force initialization? By default, `renv` will refuse
#'   to initialize a virtual environment with the home directory, to defend
#'   against accidental usages of `init()`.
#'
#' @export
init <- function(project = NULL, force = FALSE) {
  project <- project %||% getwd()
  renv_init_validate_project(project, force)
  setwd(project)
  hydrate(project)
  snapshot(project, confirm = FALSE)
  activate(project)
}

renv_init_validate_project <- function(project, force) {

  # allow all project directories when force = TRUE
  if (force) return(TRUE)

  # disallow attempts to initialize renv in the home directory
  home <- path.expand("~/")
  msg <- if (renv_file_same(project, home))
    "refusing to initialize project in home directory"
  else if (path_within(home, project))
    sprintf("refusing to initialize project in directory '%s'", project)

  if (!is.null(msg)) {
    msg <- paste(msg, "(use 'force = TRUE' to override)")
    stopf(msg)
  }

}

