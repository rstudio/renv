
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
#' **Name** \tab **Description** \cr
#'
#' `auto.snapshot` \tab `logical(1)` \tab
#'   Automatically snapshot changes to the project library after a new package is installed?
#'   Note that package upgrades or removals will not be automatically snapshotted.
#'   (Boolean; defaults to `TRUE`) \cr
#'
#' `sandbox.enabled` \tab `logical(1)` \tab
#'   Enable sandboxing for `renv` projects? When active, `renv` will attempt to sandbox
#'   the system library, preventing user-installed packages from becoming available in
#'   `renv` projects. (Boolean; defaults to `FALSE`) \cr
#'
#' `use.cache` \tab `logical(1)` \tab
#'   Use the global cache when installing packages?
#'   (Boolean; defaults to `TRUE`) \cr
#'
#' }
#'
#' @rdname config
#' @name config
NULL

renv_config_get <- function(name, default = NULL) {

  # check for R option of associated name
  optname <- tolower(name)
  optkey <- paste("renv.config", optname, sep = ".")
  optval <- getOption(optkey)
  if (!is.null(optval))
    return(optval)

  # check for environment variable
  envname <- gsub(".", "_", toupper(name), fixed = TRUE)
  envkey <- paste("RENV_CONFIG", envname, sep = "_")
  envval <- Sys.getenv(envkey, unset = NA)
  if (!is.na(envval))
    return(renv_config_decode_envvar(envname, envval))

  # return default if nothing found
  default

}

renv_config_decode_envvar <- function(envname, envval) {

  map <- renv_global("config.map", env(
    "NULL" = NULL,
    "NA"   = NA,
    "NaN"  = NaN,
    "true" = TRUE,
    "TRUE" = TRUE,
    "false" = FALSE,
    "FALSE" = FALSE
  ))

  if (exists(envval, envir = map, inherits = FALSE))
    return(get(envval, envir = map, inherits = FALSE))

  strsplit(envval, "\\s*,\\s*")[[1]]

}
