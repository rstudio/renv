
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
#' Note that if both the \R option and the environment variable are defined, the
#' \R option will be used instead. Environment variables can be more useful when
#' you want a particular configuration to be automatically inherited by child
#' processes; if that behavior is not desired, then the R option may be
#' preferred.
#'
#' If you want to set and persist these options across multiple projects, it is
#' recommended that you set them in a a startup `.Renviron` file; e.g. in your
#' own `~/.Renviron`, or in the R installation's `etc/Rprofile.site` file. See
#' [Startup] for more details.
#'
#' Configuration options can also be set within the project `.Rprofile`, but
#' be aware the options should be set before `source("renv/activate.R")` is
#' called.
#'
#' @eval renv_roxygen_config_section()
#'
#' @section Copy Methods:
#'
#' If you find that `renv` is unable to copy some directories in your
#' environment, you may want to try setting the `copy.method` option. By
#' default, `renv` will try to choose a system tool that is likely to succeed in
#' copying files on your system -- `robocopy` on Windows, and `cp` on Unix.
#' `renv` will also instruct these tools to preserve timestamps and attributes
#' when copying files. However, you can select a different method as
#' appropriate.
#'
#' The following methods are supported:
#'
#' \tabular{ll}{
#' `auto`     \tab Use `robocopy` on Windows, and `cp` on Unix-alikes. \cr
#' `R`        \tab Use \R's built-in `file.copy()` function. \cr
#' `cp`       \tab Use `cp` to copy files. \cr
#' `robocopy` \tab Use `robocopy` to copy files. (Only available on Windows.) \cr
#' `rsync`    \tab Use `rsync` to copy files. \cr
#' }
#'
#' You can also provide a custom copy method if required; e.g.
#'
#' ```
#' options(renv.config.copy.method = function(src, dst) {
#'   # copy a file from 'src' to 'dst'
#' })
#' ```
#'
#' Note that `renv` will always first attempt to copy a directory first to a
#' temporary path within the target folder, and then rename that temporary path
#' to the final target destination. This helps avoid issues where a failed
#' attempt to copy a directory could leave a half-copied directory behind
#' in the final location.
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
#' Sys.setenv(RENV_CONFIG_AUTO_SNAPSHOT = FALSE)
#'
#' @rdname config
#' @name config
NULL

renv_config_get <- function(name,
                            scope   = "config",
                            type    = "*",
                            default = NULL,
                            args    = NULL)
{
  # check for R option of associated name
  optname <- tolower(name)
  optkey <- paste("renv", scope, optname, sep = ".")
  optval <- getOption(optkey)
  if (!is.null(optval))
    return(renv_config_validate(name, optval, type, default, args))

  # check for environment variable
  envname <- gsub(".", "_", toupper(name), fixed = TRUE)
  envkey <- paste("RENV", toupper(scope), envname, sep = "_")
  envval <- Sys.getenv(envkey, unset = NA)
  if (!is.na(envval) && nzchar(envval)) {
    decoded <- renv_config_decode_envvar(envkey, envval)
    return(renv_config_validate(name, decoded, type, default, args))
  }

  # return default if nothing found
  default

}

renv_config_decode_envvar <- function(envname, envval) {

  map <- env(
    "NULL" = NULL,
    "NA"   = NA,
    "NaN"  = NaN,
    "true" = TRUE,
    "True" = TRUE,
    "TRUE" = TRUE,
    "false" = FALSE,
    "False" = FALSE,
    "FALSE" = FALSE
  )

  if (exists(envval, envir = map, inherits = FALSE))
    return(get(envval, envir = map, inherits = FALSE))

  libvars <- c("RENV_CONFIG_EXTERNAL_LIBRARIES", "RENV_CONFIG_HYDRATE_LIBPATHS")
  pattern <- if (envname %in% libvars)
    "\\s*[:;,]\\s*"
  else
    "\\s*,\\s*"

  strsplit(envval, pattern, perl = TRUE)[[1L]]

}

renv_config_validate <- function(name, value, type, default, args) {

  # no validation required for type = '*'
  if (identical(type, "*"))
    return(value)

  # if 'value' is a function, invoke it with args
  if (is.function(value)) {
    value <- catch(do.call(value, args))
    if (inherits(value, "error")) {
      warning(value, call. = FALSE)
      return(default)
    }
  }

  # parse the type string
  pattern <- paste0(
    "^",          # start of specifier
    "([^[(]+)",   # type name
    "[[(]",       # opening bracket
    "([^])]+)",   # length specifier
    "[])]",       # closing bracket
    "$"           # end of specifier
  )

  m <- regexec(pattern, type)
  matches <- regmatches(type, m)
  fields <- matches[[1L]]

  # extract declared mode, size
  mode <- fields[[2L]]
  size <- fields[[3L]]

  # validate the requested size for this option
  if (!renv_config_validate_size(value, size)) {
    fmt <- "value for option '%s' does not satisfy constraint '%s'"
    warningf(fmt, name, type)
  }

  # convert NULL values to requested type
  if (is.null(value)) {
    value <- convert(value, mode)
    return(value)
  }

  # otherwise, validate that this is a valid option
  if (identical(storage.mode(value), mode))
    return(value)

  # try converting
  converted <- catchall(convert(value, mode))
  if (any(is.na(converted)) || inherits(converted, "condition")) {
    fmt <- "'%s' does not satisfy constraint '%s' for config '%s'; using default '%s' instead"
    warningf(fmt, stringify(value), type, name, stringify(default))
    return(default)
  }

  # ok, validated + converted option
  converted

}

renv_config_validate_size <- function(value, size) {

  case(
    size == "*" ~ TRUE,
    size == "+" ~ length(value) > 0,
    size == "?" ~ length(value) %in% c(0, 1),
    TRUE        ~ as.numeric(size) == length(value)
  )

}

renv_config_install_staged <- function(default = TRUE) {

  values <- c(
    config$install.staged(default = NULL),
    config$install.transactional(default = NULL),
    default
  )

  values[[1L]]

}
