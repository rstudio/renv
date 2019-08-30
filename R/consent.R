
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
#' can be taken. Please call `renv::consent()` in an interactive session to
#' provide this consent.
#'
#' You can also set the \R option:
#'
#' ```
#' options(renv.consent = TRUE)
#' ```
#'
#' to implicitly provide consent for e.g. non-interactive \R sessions.
#'
#' @export
consent <- function() {

  if (!interactive()) {
    msg <- "renv::consent() must be called in an interactive session"
    stop(msg, call. = FALSE)
  }

  root <- renv_paths_root()

  template <- system.file("resources/WELCOME", package = "renv")
  contents <- readLines(template)
  replacements <- list(RENV_PATHS_ROOT = shQuote(aliased_path(root)))
  welcome <- renv_template_replace(contents, replacements)
  writeLines(welcome)

  response <- catchall(proceed(default = FALSE))
  if (!identical(response, TRUE)) {
    msg <- "consent was not provided; operation aborted"
    stop(msg, call. = FALSE)
  }

  options(renv.consent = TRUE)
  return(TRUE)

}

renv_consent_check <- function() {

  # check for explicit consent
  consent <- getOption("renv.consent")
  if (identical(consent, TRUE))
    return(TRUE)
  else if (identical(consent, FALSE))
    stop("user has explicitly withdrawn consent from renv", call. = FALSE)

  # check for implicit consent
  root <- renv_paths_root()
  consented <-
    !is.na(Sys.getenv("CI", unset = NA)) ||
    !is.na(Sys.getenv("RENV_PATHS_ROOT", unset = NA)) ||
    file.exists(root)

  if (consented)
    return(TRUE)

  if (!interactive()) {
    msg <- "please call `renv::consent()` in an interactive R session to continue"
    stop(msg, call. = FALSE)
  }

  consent()

}
