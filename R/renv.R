
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
create <- function(name,
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
activate <- function(name = NULL, project = NULL) {
  project <- renv_active_project(project)

  if (is.null(name)) {
    state <- file.path(project, "renv/renv.dcf")
    if (file.exists(state))
      name <- read.dcf(state, fields = "Environment")
  }

  ensure_existing_renv(name)
  renv_write_infrastructure(project, name)
  renv_bootstrap()

  renv_verbose_with(FALSE, snapshot(name, confirm = FALSE))

  if (renv_verbose()) {
    fmt <- "* Activating %s environment '%s' ..."
    messagef(fmt, if (renv_local()) "local virtual" else "virtual", name)
  }

  # set library paths now so that they're properly restored in new sessions
  manifest <- renv_manifest_load(project)
  renv_load_libpaths(manifest)

  reason <- sprintf("Virtual environment '%s' activated.", name)
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
  project <- renv_active_project(project)

  renv_remove_rprofile(project)

  state <- file.path(project, "renv/renv.dcf")
  if (!file.exists(state)) {
    fmt <- "Project '%s' has no active virtual environment."
    stopf(fmt, aliased_path(project))
  }

  name <- read.dcf(state, fields = "Environment")
  if (renv_verbose()) {
    fmt <- "* Deactivating %s environment '%s' ..."
    messagef(fmt, if (renv_local()) "local virtual" else "virtual", name)
  }

  Sys.setenv(
    R_LIBS_USER = Sys.getenv("RENV_DEFAULT_R_LIBS_USER"),
    R_LIBS_SITE = Sys.getenv("RENV_DEFAULT_R_LIBS_SITE"),
    R_LIBS      = Sys.getenv("RENV_DEFAULT_R_LIBS")
  )

  libpaths <- Sys.getenv("RENV_DEFAULT_LIBPATHS")
  .libPaths(strsplit(libpaths, .Platform$path.sep, fixed = TRUE)[[1]])

  env <- Sys.getenv()
  stale <- grep("^RENV_", names(env), value = TRUE)
  Sys.unsetenv(stale)

  renv_request_restart("renv deactivated")
}

#' Load an R Virtual Environment.
#'
#' Load the \R virtual environment currently associated with a project.
#'
#' Normally, this is done automatically on session startup by the infrastructure
#' loadd by [activate()] -- users should not need to call this
#' function directly.
#'
#' @inheritParams renv-params
#'
#' @export
load <- function(project = NULL) {

  project <- renv_active_project(project)
  renv <- renv_load_project(project)

  path <- ensure_existing_renv(renv)
  spec <- renv_manifest_read(path)

  renv_set_active_project(project)
  renv_set_active_environment(renv)

  renv_load_r_version(spec)
  renv_load_repos(spec)
  renv_load_libpaths(spec)

  renv_load_report()


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
  name <- renv_active_environment(name)
  path <- ensure_existing_renv(name)
  file.edit(path)
}

# TODO
# #' Remove a Virtual Environment
# #'
# #' Remove an existing virtual environment.
# #'
# #' @inheritParams renv-params
# #'
# #' @export
# remove <- function(name = NULL) {
#   name <- renv_active_environment(name)
#   path <- ensure_existing_renv(name)
#   unlink(path, recursive = TRUE)
#   fmt <- "* %s environment '%s' successfully removed."
#   messagef(fmt, if (renv_local()) "Local virtual" else "Virtual", name)
#   invisible(name)
# }
