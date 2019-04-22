
#' User-Level Configuration of renv
#'
#' Configure different behaviors of `renv`.
#'
#' For a given configuration option:
#'
#' 1. If an \R option of the form `renv.config.<name>` is available,
#'    then that option's value will be used;
#'
#' 2. If an environment variable of the form `RENV_CONFIG_<NAME>` is available,
#'   then that option's value will be used;
#'
#' 3. Otherwise, the default for that particular configuration value is used.
#'
#' Any periods (`.`)s in the option name are transformed into underscores (`_`)
#' in the environment variable name, and vice versa. For example, the
#' configuration option `auto.snapshot` could be configured as:
#'
#' - `options(renv.config.auto.snapshot = <...>)`
#' - `Sys.setenv(RENV_CONFIG_AUTO_SNAPSHOT = <...>)`
#'
#' Note that if both the R option and the environment variable are defined, the
#' R option will be used instead. Environment variables can be more useful when
#' you want a particular configuration to be automatically inherited by child
#' processes; if that behavior is not desired, then the R option may be
#' preferred.
#'
#' If you want to set and persist these options across multiple projects, it is
#' recommended that you set them in your user startup files (e.g. in
#' `~/.Rprofile`).
#'
#' @section Configuration:
#'
#' The following `renv` configuration options are available:
#'
#' \tabular{lll}{
#' **Name**        \tab **Type**      \tab **Description** \cr
#' `auto.snapshot` \tab `logical(1)`  \tab Automatically snapshot changes to the project library after a call to [install()]. \cr
#' `use.cache`     \tab `logical(1)`  \tab Use the global cache when installing packages?
#' }
#'
#' @rdname config
#' @name config
NULL

renv_config <- function(name, default, coerce = identity) {

  result <- catch(renv_config_impl(name, default, coerce))
  if (inherits(result, "error")) {
    warning(result)
    return(default)
  }

  result

}

renv_config_impl <- function(name, default, coerce) {

  optname <- tolower(name)
  optkey <- paste("renv.config", optname, sep = ".")
  optval <- getOption(optkey)
  if (!is.null(optval))
    return(coerce(optval))

  envname <- gsub(".", "_", toupper(name), fixed = TRUE)
  envkey <- paste("RENV_CONFIG", envname, sep = "_")
  envval <- Sys.getenv(envkey, unset = NA)
  if (!is.na(envval))
    return(coerce(envval))

  default

}

renv_config_auto_snapshot <- function() {
  renv_config("auto.snapshot", TRUE, as.logical)
}

renv_config_use_cache <- function() {
  renv_config("use.cache", TRUE, as.logical)
}
