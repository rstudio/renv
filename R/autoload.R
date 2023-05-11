
#' Auto-load the Active Project
#'
#' Automatically load the `renv` project associated with a particular directory.
#' `renv` will search parent directories for the `renv` project root; if found,
#' that project will be loaded via [renv::load()].
#'
#' To enable the `renv` auto-loader, you can place:
#'
#' ```
#' renv::autoload()
#' ````
#'
#' into your site-wide or user `.Rprofile` to ensure that `renv` projects are
#' automatically loaded for any newly-launched \R sessions, even if those \R
#' sessions are launched within the sub-directory of an `renv` project.
#'
#' If you'd like to launch \R within the sub-directory of an `renv` project
#' without auto-loading `renv`, you can set the environment variable:
#'
#' ```
#' RENV_AUTOLOAD_ENABLED = FALSE
#' ```
#'
#' before starting \R.
#'
#' Note that `renv::autoload()` is only compatible with projects using
#' `renv 0.15.3` or newer, as it relies on features within the `renv/activate.R`
#' script that are only generated with newer versions of `renv`.
#'
#' @export
autoload <- function() {
  invisible(renv_autoload_impl())
}

renv_autoload_impl <- function() {

  # check if we're disabled
  enabled <- Sys.getenv("RENV_AUTOLOAD_ENABLED", unset = "TRUE")
  if (falsy(enabled, FALSE))
    return(FALSE)

  # bail if load is already being called
  loading <- getOption("renv.load.running")
  if (identical(loading, TRUE))
    return(FALSE)

  # avoid recursion
  running <- getOption("renv.autoload.running")
  if (identical(running, TRUE))
    return(FALSE)

  # set our flag
  renv_scope_options(renv.autoload.running = TRUE)

  # try to find a project
  project <- catch(renv_project_find())
  if (inherits(project, "error"))
    return(FALSE)

  # move to project directory
  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  # if we have a project profile, source it
  profile <- file.path(project, ".Rprofile")
  if (file.exists(profile)) {
    sys.source(profile, envir = globalenv())
    return(TRUE)
  }

  # if we have an activate script, run it
  activate <- file.path(project, "renv/activate.R")
  if (file.exists(activate)) {
    sys.source(activate, envir = globalenv())
    return(TRUE)
  }

  # otherwise, just try to load the project
  load(project)
  TRUE

}

# TODO: this gets really dicey once the user starts configuring where
# renv places its project-local state ...
renv_project_find <- function(project = NULL) {

  project <- project %||% getwd()

  anchors <- c("renv.lock", "renv/activate.R")
  resolved <- renv_file_find(project, function(parent) {
    for (anchor in anchors)
      if (file.exists(file.path(parent, anchor)))
        return(parent)
  })

  if (is.null(resolved)) {
    fmt <- "couldn't resolve renv project associated with path %s"
    stopf(fmt, renv_path_pretty(project))
  }

  resolved
}
