
#' Initialize a Project-local Virtual Environment
#'
#' Discover packages used within the current project, and then initialize a
#' project-local virtual environment with those packages. The
#' currently-installed versions of any packages in use (as detected within the
#' user library) are then added to the project manifest, effectively forking
#' the state of your user library into a private project library.
#'
#' The primary steps taken when initializing a new virtual environment are:
#'
#' 1. \R package dependencies are discovered within the \R files used within
#'    the project;
#'
#' 2. Discovered packages are copied into the `renv` global package cache (so
#'    these packages can be re-used across multiple projects as necessary),
#'
#' 3. Any missing \R package dependencies discovered are then installed into
#'    a private project library,
#'
#' 4. The [activate()] function is called to activate the newly-created
#'    virtual environment.
#'
#' This mimics the workflow provided by `packrat::init()`, but with more
#' reasonable default behaviors -- in particular, `renv` does not attempt
#' to download and store package sources, and `renv` will re-use packages
#' that have already been installed whenever possible.
#'
#' @param project The project directory.
#' @param ... Optional arguments passed to [create()].
#' @param force Boolean; force initialization? By default, `renv` will refuse
#'   to initialize the home directory, or parents of the home directory.
#'
#' @export
init <- function(project = NULL, ..., force = FALSE) {
  project <- project %||% getwd()

  # ensure this is a valid project directory
  renv_init_validate_project(project, force)

  # switch to local mode
  renv_state$local(TRUE)

  # create the virtual environment
  # TODO: what action to take if environment already exists?
  # - re-create the environment and re-run the dependency discovery + caching?
  # - skip those steps and just re-use the existing environment?
  name <- basename(project)
  create(name = name, r_libs = name, ..., overwrite = TRUE)

  # hydrate the newly-created environment
  hydrate(name, project)

  # now we can activate the local environment
  activate(name, project)

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

