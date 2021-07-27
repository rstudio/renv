
#' Auto-load the Active Project
#'
#' Automatically load the `renv` project associated with a particular directory.
#' `renv` will search parent directories for the `renv` project root; if found,
#' that project will be loaded via [renv::load()].
#'
#' Place `renv::autoload()` into your site-wide or user `.Rprofile` to ensure
#' that `renv` projects are automatically loaded for any newly-launched \R
#' sessions, even if those \R sessions are launched within the sub-directory
#' of an `renv` project.
#'
#' If you'd like to launch \R within the sub-directory of an `renv` project
#' without auto-loading `renv`, you can set the environment variable:
#'
#' ```
#' RENV_AUTOLOAD_ENABLED = FALSE
#' ```
#'
#' before starting \R.
autoload <- function() {

  enabled <- Sys.getenv("RENV_AUTOLOADER_ENABLED", unset = "TRUE")
  if (falsy(enabled, FALSE))
    return(invisible(NULL))

  project <- catch(renv_project_find())
  if (inherits(project, "error"))
    return(invisible(NULL))

  load(project)

}
