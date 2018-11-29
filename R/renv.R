#' Create an renv
#'
#' Create an `renv` environment.
#'
#' @param name The name of the environment to be created.
#' @param config The configuration for this `renv`. See [config]
#'   for more details.
#'
#' @export
renv_create <- function(name, config = renv_config()) {

  path <- ensure_no_renv(name)
  ensure_directory(dirname(path))
  renv_bootstrap()
  renv_config_write(config, path)

  if (renv_verbose()) {
    fmt <- "* Created renv at path '%s'."
    messagef(fmt, aliased_path(path))
  }

  invisible(name)
}


#' Activate an renv
#'
#' Activate an `renv`. This binds a project to an existing `renv`
#' environment definition. Newly-launched \R sessions in this project will
#' use the `renv` as specified by `name`.
#'
#' @param name The name of the environment you'd like to activate.
#' @param project The project directory.
#'
#' @export
renv_activate <- function(name, project = ".") {
  ensure_existing_renv(name)

  renv_write_infrastructure(project, name)

  if (renv_verbose()) {
    fmt <- "* Activating renv '%s' ..."
    messagef(fmt, name)
  }

  reason <- sprintf("renv '%s' activated", name)
  renv_request_restart(reason)
}

#' Deactivate an renv
#'
#' Deactivate the active `renv`.
#'
#' @param project The project directory.
#'
#' @export
renv_deactivate <- function(project = ".") {

  active <- file.path(project, ".renv/active")
  if (file.exists(active)) {
    name <- readLines(active, warn = FALSE)
    if (renv_verbose()) {
      fmt <- "* Deactivating renv '%s' ..."
      messagef(fmt, name)
    }
  }

  renv_remove_infrastructure()
  renv_request_restart("renv deactivated")
}

#' Load an renv
#'
#' Load the `renv` environment called `name`. Normally this is done
#' automatically when \R sessions are launched, after calling [activate]
#' to bind a project to a particular `renv` environment.
#'
#' @param name The name of the environment you'd like to activate.
#'
#' @export
renv_load <- function(name, project = ".") {

  path <- ensure_existing_renv(name)
  config <- renv_config_read(path)

  # check R version (note that it is the responsibility of the user,
  # or the front-end, to ensure that R versions match)
  version <- config$r_version
  if (!is_compatible_version(version, getRversion())) {
    fmt <- "renv '%s' requested R version '%s' but '%s' is currently being used"
    warningf(fmt, name, version, getRversion())
  }

  # prepare the library paths
  renv <- file.path(renv_paths_renv(), sprintf("renv-%s", config$renv_version))
  libs <- rev(renv_paths_lib(config$r_libs))
  lapply(libs, ensure_directory)

  libpaths <- c(libs, renv, if (config$r_libs_overlay) .libPaths())
  .libPaths(libpaths)

  # report environment changes to the user
  if (renv_verbose()) {

    fmt <- "* renv '%s' loaded. Using library paths:"
    messagef(fmt, name)

    paths <- paste("-", shQuote(.libPaths()), collapse = "\n")
    message(paths)

  }

  invisible(name)
}

#' @export
renv_edit <- function(name) {
  path <- ensure_existing_renv(name)
  file.edit(path)
}
