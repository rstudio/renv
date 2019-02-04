
#' Create an R Virtual Environment
#'
#' Create a new virtual environment.
#'
#' @param name           `character[1]`: The name of the virtual environment.
#' @param r_version      `character[1]`: The version of \R to be used.
#' @param r_repos        `character[*]`: The \R repositories to use with this project.
#' @param r_libs         `character[*]`: The \R libraries associated with this environment.
#' @param r_libs_overlay `logical[1]`:   Overlay `r_libs` on top of the default \R libraries?
#' @param python         `character[1]`: The (optional) path to a Python binary, or virtual environment.
#' @param ... Optional arguments; currently unused.
#'
#' @param overwrite Boolean; overwrite a pre-existing virtual environment?
#' @param local Boolean; use a project-local virtual environment?
#'
#' @family renv
#'
#' @export
create <- function(name,
                   r_version      = getRversion(),
                   r_repos        = getOption("repos"),
                   r_libs         = NULL,
                   r_libs_overlay = FALSE,
                   python         = NULL,
                   ...,
                   overwrite      = FALSE,
                   local          = NULL)
{
  local <- local %||% renv_state$local()
  renv_state$local(local, scoped = TRUE)

  # generate path to renv
  path <- renv_paths_environment(name)
  if (!overwrite && renv_file_exists(path)) {
    fmt <- "%s environment '%s' already exists."
    stopf(fmt, if (renv_state$local()) "Local virtual" else "Virtual", name)
  }

  # normalize name as path if it was given as such
  ensure_parent_directory(path)
  if (grepl("[/\\]", name))
    name <- renv_file_normalize(name, winslash = "/", mustWork = TRUE)

  # generate library path if none provided
  r_libs <- r_libs %||%
    basename(tempfile("renv-library-", tmpdir = renv_paths_library()))

  # construct blueprint for environment
  blueprint <- list()

  blueprint$Environment <- list(
    Environment = name
  )

  blueprint$R <- list(
    Version      = r_version,
    Library      = r_libs,
    Overlay      = r_libs_overlay,
    Repositories = r_repos
  )

  blueprint$Python <- renv_python_blueprint_resolve(python)

  ensure_parent_directory(path)
  renv_manifest_write(blueprint, path)

  fmt <- "* Created %s environment '%s'."
  local <- renv_state$local()
  vmessagef(fmt, if (local) "local virtual" else "virtual", name)

  invisible(blueprint)
}


#' Activate an R Virtual Environment
#'
#' Activate an \R virtual environment. This binds the requested project to the
#' virtual environment called `name`. Newly-launched \R sessions in this project
#' will use this virtual environment.
#'
#' @inheritParams renv-params
#'
#' @param local Boolean; activate a project-local environment?
#'
#' @family renv
#'
#' @export
activate <- function(name = NULL, project = NULL, local = NULL) {

  local <- local %||% renv_state$local()
  project <- project %||% renv_state$project()

  if (is.null(name)) {

    # if we weren't provided with an environment name, take this as a request
    # to re-activate a previously-activated virtual environment (if any)
    activate <- renv_activate_read(project)
    name <- activate$Environment
    local <- activate$Local
    renv_state$local(local)

  } else {

    # we were provided with an environment name; respect user's request for
    # local vs. non-local environment
    renv_state$local(local, scoped = TRUE)

  }

  # prepare renv infrastructure
  ensure_existing_renv(name)
  renv_write_infrastructure(project, name)
  renv_bootstrap()

  # generate a manifest (so that activating an environment implies
  # using the current state of the environment)
  # TODO: or should we skip this and use the last-generated manifest?
  manifest <- snapshot(name, file = NULL)
  file <- renv_snapshot_manifest_path(project)
  ensure_parent_directory(file)
  renv_manifest_write(manifest, file = file)

  fmt <- "* Activating %s environment '%s' ..."
  vmessagef(fmt, if (renv_state$local()) "local virtual" else "virtual", name)

  # set library paths now so that they're properly restored in new sessions
  manifest <- renv_manifest_load(project)
  renv_load_libpaths(manifest)
  renv_load_prompt(manifest)

  reason <- sprintf("Virtual environment '%s' activated", name)
  renv_request_restart(reason)

}

#' Deactivate an R Virtual Environment
#'
#' Deactivate the active virtual environment.
#'
#' @inheritParams renv-params
#'
#' @family renv
#'
#' @export
deactivate <- function(project = NULL) {
  project <- project %||% renv_state$project()

  renv_remove_rprofile(project)
  renv_envvars_restore()

  activate <- renv_activate_read(project)
  name <- activate$Environment
  fmt <- "* Deactivating %s environment '%s' ..."
  vmessagef(fmt, if (renv_state$local()) "local virtual" else "virtual", name)
  renv_reset_prompt()
  renv_request_restart("Virtual environment deactivated")
}

#' Load an R Virtual Environment
#'
#' Load the \R virtual environment currently associated with a project.
#'
#' Normally, this is done automatically on session startup by the infrastructure
#' generated by [activate()] -- users should not need to call this function
#' directly.
#'
#' @inheritParams renv-params
#'
#' @export
load <- function(project = NULL) {

  project <- project %||% renv_state$project()
  renv <- renv_load_project(project)

  path <- ensure_existing_renv(renv)
  spec <- renv_manifest_read(path)

  local <- renv_activate_read(project, field = "Local")

  renv_state$project(project)
  renv_state$environment(renv)
  renv_state$local(local)

  renv_envvars_save()

  renv_load_r_version(spec)
  renv_load_envvars(spec)
  renv_load_libpaths(spec)
  renv_load_repos(spec)
  renv_load_python(spec)
  renv_load_prompt(spec)
  renv_load_finish()

  invisible(renv)

}

#' Modify an R Virtual Environment
#'
#' Modify the configuration file associated with an \R virtual environment. This
#' can be useful if you need to manually tweak or edit values in a pre-existing
#' environment.
#'
#' @inheritParams renv-params
#'
#' @family renv
#'
#' @export
modify <- function(name = NULL) {
  name <- name %||% renv_state$environment()
  path <- ensure_existing_renv(name)
  file.edit(path)
}

