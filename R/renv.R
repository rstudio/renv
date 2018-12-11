
#' Create an R Virtual Environment.
#'
#' Create a new virtual environment.
#'
#' @param name           `character[1]`: The name of the virtual environment.
#' @param r_version      `character[1]`: The version of \R to be used.
#' @param r_repos        `character[*]`: The \R repositories to use with this project.
#' @param r_libs         `character[*]`: The \R libraries associated with this environment.
#' @param r_libs_overlay `logical[1]`:   Overlay `r_libs` on top of the default \R libraries?
#'
#' @family renv
#'
#' @export
renv_create <- function(name,
                        r_version      = getRversion(),
                        r_repos        = getOption("repos"),
                        r_libs         = character(),
                        r_libs_overlay = FALSE)
{
  blueprint <- list(

    Environment = list(
      Name = name
    ),

    R = list(
      Version      = r_version,
      Libraries    = r_libs,
      Overlay      = r_libs_overlay,
      Repositories = r_repos
    )

  )

  path <- ensure_no_renv(name)
  ensure_parent_directory(path)
  renv_manifest_write(blueprint, path)

  if (renv_verbose()) {
    fmt <- "* Created %s environment '%s'."
    local <- renv_local()
    messagef(fmt, if (local) "local virtual" else "virtual", name)
  }

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
#' @family renv
#'
#' @export
renv_activate <- function(name, project = NULL) {
  project <- renv_active_project(project)

  ensure_existing_renv(name)
  renv_write_infrastructure(project, name)

  if (renv_verbose()) {
    fmt <- "* Activating %s environment '%s' ..."
    messagef(fmt, if (renv_local()) "local virtual" else "virtual", name)
  }

  reason <- sprintf("renv '%s' activated", name)
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
renv_deactivate <- function(project = NULL) {
  project <- renv_active_project(project)

  active <- file.path(project, "renv/active")
  if (file.exists(active)) {
    dcf <- read.dcf(active, all = TRUE)
    if (renv_verbose()) {
      fmt <- "* Deactivating %s environment '%s' ..."
      messagef(fmt, if (renv_local()) "local virtual" else "virtual", dcf$name)
    }
  }

  renv_remove_infrastructure()
  renv_request_restart("renv deactivated")
}

#' Load an R Virtual Environment.
#'
#' Load the \R virtual environment currently associated with a project.
#'
#' Normally, this is done automatically on session startup by the infrastructure
#' initialized by [renv_activate()] -- users should not need to call this
#' function directly.
#'
#' @inheritParams renv-params
#'
#' @export
renv_load <- function(project = NULL) {
  project <- renv_active_project(project)
  renv <- renv_load_project(project)

  path <- ensure_existing_renv(renv)
  spec <- renv_manifest_read(path)

  renv_set_active_project(project)
  renv_set_active_renv(renv)

  renv_load_r_version(spec)
  renv_load_repos(spec)
  renv_load_libpaths(spec)

  # report environment changes to the user
  renv_load_report()

  invisible(renv)
}

#' Edit an R Virtual Environment
#'
#' Edit the configuration file associated with an \R virtual environment. This
#' can be useful if you need to manually tweak or edit values in a pre-existing
#' environment.
#'
#' @inheritParams renv-params
#'
#' @family renv
#'
#' @export
renv_edit <- function(name = NULL) {
  name <- renv_active_renv(name)
  path <- ensure_existing_renv(name)
  file.edit(path)
}

#' Remove a Virtual Environment
#'
#' Remove an existing virtual environment.
#'
#' @inheritParams renv-params
#'
#' @export
renv_remove <- function(name = NULL) {
  name <- renv_active_renv(name)
  path <- ensure_existing_renv(name)
  unlink(path, recursive = TRUE)
  fmt <- "* %s environment '%s' successfully removed."
  messagef(fmt, if (renv_local()) "Local virtual" else "Virtual", name)
  invisible(name)
}
