
`_renv_settings_defaults` <- new.env(parent = emptyenv())

renv_settings_defaults <- function() {
  as.list(`_renv_settings_defaults`)
}

renv_settings_default <- function(name) {
  `_renv_settings_defaults`[[name]]
}

renv_settings_read <- function(project) {

  path <- file.path(project, "renv/renv.opts")
  if (!renv_file_exists(path))
    return(renv_settings_defaults())

  dcf <- catch(renv_dcf_read(path))
  if (inherits(dcf, "error"))
    return(renv_settings_defaults())

  settings <- enumerate(dcf, function(name, value) {

    case(
      value == "NULL"  ~ NULL,
      value == "NA"    ~ NA,
      value == "NaN"   ~ NaN,
      value == "TRUE"  ~ TRUE,
      value == "FALSE" ~ FALSE,
      ~ strsplit(value, "\\s*,\\s*")[[1]]
    )

  })

  renv_filebacked_set(path, settings)

}

renv_settings_get <- function(project, name) {

  path <- file.path(project, "renv/renv.opts")
  cache <- renv_filebacked_get(path)
  if (!is.null(cache))
    return(cache[[name]] %||% renv_settings_default(name))

  settings <- renv_settings_read(project)
  settings[[name]] %||% renv_settings_default(name)

}

renv_settings_set <- function(project, name, value, persist = TRUE) {

  path <- file.path(project, "renv/renv.opts")

  settings <- renv_filebacked_get(path) %||% renv_settings_read(project)
  settings[[name]] <- value
  renv_filebacked_set(path, settings)

  if (persist)
    renv_settings_persist(project, settings)

}

renv_settings_persist <- function(project, settings) {
  path <- file.path(project, "renv/renv.opts")
  settings <- lapply(settings, paste, collapse = ", ")
  write.dcf(as.data.frame(settings, stringsAsFactors = FALSE), path)
}

renv_settings_merge <- function(settings, merge) {
  settings[names(merge)] <- merge
  settings
}

renv_settings_impl <- function(name, default) {

  force(name)
  `_renv_settings_defaults`[[name]] <- default

  function(value, project = NULL, persist = TRUE) {
    project <- project %||% renv_project()
    if (missing(value))
      renv_settings_get(project, name)
    else
      renv_settings_set(project, name, value, persist)
  }

}

#' Settings
#'
#' Define project-local settings that can be used to adjust the behavior of
#' `renv` with your particular project.
#'
#' @section Settings:
#'
#' \describe{
#'
#' \item{\code{ignored.packages}}{
#'
#'   A vector of packages, which should be ignored when attempting to snapshot
#'   the project's private library. Note that if a package has already been
#'   added to the lockfile, that entry in the lockfile will not be ignored. \cr
#'
#' }
#'
#' \item{\code{external.libraries}}{
#'
#'   A vector of library paths, to be used in addition to the project's own
#'   private library. This can be useful if you have a package available for use
#'   in some global library, but for some reason `renv` is not able to install
#'   that package (e.g. sources or binaries for that package are not publicly
#'   available, or you have been unable to orchestrate the pre-requisites for
#'   installing some packages from source on your machine).
#'
#' }
#'
#' \item{\code{python}}{
#'
#'   The path to a Python binary, to be used by e.g. `reticulate` for projects
#'   requiring the use of Python. Alternatively, if set to `TRUE`, then `renv`
#'   will create a project-local Python virtual environment and use that.
#'   In that case, `renv` will use whichever version of Python is currently
#'   in use by `reticulate` (if any), or the version of Python requested by
#'   `RETICULATE_PYTHON` otherwise.
#'
#' }
#'
#' }
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # check the 'ignored.packages' option
#' renv::settings$ignored.packages()
#'
#' # ignore the 'tidyverse' package in this project
#' renv::settings$ignored.packages("tidyverse")
#'
#' }
settings <- list(
  ignored.packages   = renv_settings_impl("ignored.packages", character()),
  external.libraries = renv_settings_impl("external.libraries", character()),
  python             = renv_settings_impl("python", NULL)
)
