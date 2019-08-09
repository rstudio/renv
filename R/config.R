
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
#' `~/.Rprofile` or `~/.Renviron`).
#'
#' @section Configuration:
#'
#' The following `renv` configuration options are available:
#'
#' \tabular{lll}{
#' **Name** \tab **Type** \tab **Description** \cr
#'
#' `auto.snapshot` \tab `logical(1)` \tab
#'   Automatically snapshot changes to the project library after a new package
#'   is installed with `renv::install()`, or removed with `renv::remove()`?
#'   (Boolean; defaults to `TRUE`) \cr
#'
#' `connect.timeout` \tab `integer(1)` \tab
#'   The amount of time to spend (in seconds) when attempting to download a
#'   file. If you have already configured the R downloader (that is, you have
#'   set the `download.file.method` or `download.file.extra` \R options), then
#'   this setting will be ignored.
#'   (Integer; defaults to `20`) \cr
#'
#' `connect.retry` \tab `integer(1)` \tab
#'   The number of times to attempt re-downloading a file, when transient
#'   errors occur. If you have already configured the R downloader (that is,
#'   you have set the `download.file.method` or `download.file.extra` \R
#'   options), then this setting will be ignored.
#'   (Integer; defaults to `3`) \cr
#'
#' `sandbox.enabled` \tab `logical(1)` \tab
#'   Enable sandboxing for `renv` projects? When active, `renv` will attempt to
#'   sandbox the system library, preventing user-installed packages in the
#'   system library from becoming available in `renv` projects.
#'   (Boolean; defaults to `FALSE`) \cr
#'
#' `shims.enabled` \tab `logical(1)` \tab
#'   Should `renv` shims be installed on package load? When enabled, `renv`
#'   will install its own shims over the functions `install.packages()`,
#'   `update.packages()` and `remove.packages()`, delegating these functions
#'   to `renv::install()`, `renv::update()` and `renv::remove()` as
#'   appropriate.
#'   (Boolean; defaults to `TRUE`) \cr
#'
#' `snapshot.validate` \tab `logical(1)` \tab
#'   Validate \R package dependencies when calling snapshot? When `TRUE`,
#'   `renv` will attempt to diagnose potential issues in the project library
#'   before creating `renv.lock` -- for example, if a package installed in the
#'   project library depends on a package which is not currently installed.
#'   (Boolean; defaults to `TRUE`) \cr
#'
#' }
#'
#' @section Project-Local Settings:
#'
#' For settings that should persist alongside a particular project, the
#' various settings available in [settings] can be used.
#'
#' @examples
#'
#' # disable automatic snapshots
#' options(renv.config.auto.snapshot = FALSE)
#'
#' # disable with environment variable
#' Sys.setenv(RENV_CONFIG_AUTO_SNAPSHOT = "FALSE")
#'
#' @rdname config
#' @name config
NULL

renv_config <- function(name, default = NULL) {

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
    "True" = TRUE,
    "TRUE" = TRUE,
    "false" = FALSE,
    "False" = FALSE,
    "FALSE" = FALSE
  ))

  if (exists(envval, envir = map, inherits = FALSE))
    return(get(envval, envir = map, inherits = FALSE))

  strsplit(envval, "\\s*,\\s*")[[1]]

}
