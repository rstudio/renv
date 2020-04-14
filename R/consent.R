
#' Consent to usage of renv
#'
#' Provide consent to `renv`, allowing it to write and update certain files
#' on your filesystem.
#'
#' As part of its normal operation, `renv` will write and update some files
#' in your project directory, as well as an application-specific cache
#' directory. These paths are documented within [paths].
#'
#' In accordance with the
#' [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html),
#' `renv` must first obtain consent from you, the user, before these actions
#' can be taken. Please call `renv::consent()` first to provide this consent.
#'
#' You can also set the \R option:
#'
#' ```
#' options(renv.consent = TRUE)
#' ```
#'
#' to implicitly provide consent for e.g. non-interactive \R sessions.
#'
#' @param provided The default provided response. If you need to provide
#'   consent from a non-interactive \R session, you can invoke
#'   `renv::consent(provided = TRUE)` explicitly.
#'
#' @return `TRUE` if consent is provided, or an \R error otherwise.
#'
#' @export
consent <- function(provided = FALSE) {

  renv_scope_options(renv.consenting = TRUE)

  root <- renv_paths_root()
  if (renv_file_type(root) == "directory") {
    vwritef("* Consent to use renv has already been provided -- nothing to do.")
    return(invisible(TRUE))
  }

  template <- system.file("resources/WELCOME", package = "renv")
  contents <- readLines(template)
  replacements <- list(RENV_PATHS_ROOT = shQuote(aliased_path(root)))
  welcome <- renv_template_replace(contents, replacements)
  writeLines(welcome)

  response <- catchall(proceed(default = provided))
  if (!identical(response, TRUE)) {
    msg <- "consent was not provided; operation aborted"
    stop(msg, call. = FALSE)
  }

  options(renv.consent = TRUE)
  ensure_directory(root)
  vwritef("* %s has been created.", shQuote(aliased_path(root)))

  return(invisible(TRUE))

}

renv_consent_check <- function() {

  # check for explicit consent
  consent <- getOption("renv.consent")
  if (identical(consent, TRUE))
    return(TRUE)
  else if (identical(consent, FALSE))
    stopf("consent has been explicitly withdrawn")

  # check for existence of root
  renv_scope_options(renv.consenting = TRUE)
  root <- renv_paths_root()
  if (renv_file_type(root) == "directory")
    return(TRUE)

  # check for implicit consent
  consented <-
    !is.na(Sys.getenv("CI", unset = NA)) ||
    !is.na(Sys.getenv("GITHUB_ACTION", unset = NA)) ||
    !is.na(Sys.getenv("RENV_PATHS_ROOT", unset = NA)) ||
    file.exists("/.dockerenv")

  if (consented)
    return(TRUE)

  if (!interactive()) {
    msg <- "please call `renv::consent()` before using renv"
    stop(msg, call. = FALSE)
  }

  consent()

}
