
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
#' \tabular{llll}{
#' **Name** \tab **Type** \tab **Default** \tab **Description** \cr
#'
#' `auto.snapshot` \tab `logical[1]` \tab `FALSE` \tab
#'   Automatically snapshot changes to the project library after a new package
#'   is installed with `renv::install()`, or removed with `renv::remove()`?
#'   \cr
#'
#' `connect.timeout` \tab `integer[1]` \tab `20L` \tab
#'   The amount of time to spend (in seconds) when attempting to download a
#'   file. Only used when the `curl` downloader is used.
#'   \cr
#'
#' `connect.retry` \tab `integer[1]` \tab `3L` \tab
#'   The number of times to attempt re-downloading a file, when transient
#'   errors occur. Only used when the `curl` downloader is used.
#'   \cr
#'
#' `external.libraries` \tab `character[*]` \tab `character()` \tab
#'   A character vector of external libraries, to be used in tandem with your
#'   projects. Be careful when using external libraries: it's possible that
#'   things can break within a project if the version(s) of packages used in
#'   your project library happen to be incompatible with packages in your
#'   external libraries; for example, if your project required `xyz 1.0` but
#'   `xyz 1.1` was present and loaded from an external library. Can also be an
#'   \R function that provides the paths to external libraries. Library paths
#'   will be expanded through [.expand_R_libs_env_var] as necessary.
#'   \cr
#'
#' `install.staged` \tab `logical[1]` \tab `TRUE` \tab
#'   Perform a staged install of packages during install and restore?
#'   When enabled, `renv` will first install packages into a temporary
#'   library, and later copy or move those packages back into the project
#'   library only if all packages were successfully downloaded and installed.
#'   This can be useful if you'd like to avoid mutating your project library
#'   if installation of one or more packages fails.
#'   \cr
#'
#' `repos.override` \tab `character[*]` \tab `NULL` \tab
#'   Override the R package repositories used during [restore]. Primarily
#'   useful for deployment / continuous integration, where you might want
#'   to enforce the usage of some set of repositories over what is defined
#'   in `renv.lock` or otherwise set by the R session.
#'   \cr
#'
#' `sandbox.enabled` \tab `logical[1]` \tab `FALSE` \tab
#'   Enable sandboxing for `renv` projects? When active, `renv` will attempt to
#'   sandbox the system library, preventing user-installed packages in the
#'   system library from becoming available in `renv` projects.
#'   \cr
#'
#' `shims.enabled` \tab `logical[1]` \tab `TRUE` \tab
#'   Should `renv` shims be installed on package load? When enabled, `renv`
#'   will install its own shims over the functions `install.packages()`,
#'   `update.packages()` and `remove.packages()`, delegating these functions
#'   to `renv::install()`, `renv::update()` and `renv::remove()` as
#'   appropriate.
#'   \cr
#'
#' `snapshot.validate` \tab `logical[1]` \tab `TRUE` \tab
#'   Validate \R package dependencies when calling snapshot? When `TRUE`,
#'   `renv` will attempt to diagnose potential issues in the project library
#'   before creating `renv.lock` -- for example, if a package installed in the
#'   project library depends on a package which is not currently installed.
#'   \cr
#'
#' `updates.check` \tab `logical[1]` \tab `FALSE` \tab
#'   Check for package updates when the session is initialized?
#'   This can be useful if you'd like to ensure that your project lockfile
#'   remains up-to-date with packages as they are released on CRAN.
#'   \cr
#'
#' `updates.parallel` \tab `integer[1]` \tab `2L` \tab
#'   Check for package updates in parallel? This can be useful when a large
#'   number of packages installed from non-CRAN remotes are installed, as
#'   these packages can then be checked for updates in parallel.
#'   \cr
#'
#' `user.library` \tab `logical[1]` \tab `FALSE` \tab
#'   Include the user library on the library paths for your projects? Note that
#'   this risks breaking project encapsulation and is not recommended for
#'   projects which you intend to share or collaborate on with other users. See
#'   also the caveats for the `external.libraries` option.
#'   \cr
#'
#' `user.profile` \tab `logical[1]` \tab `TRUE` \tab
#'   Load the user R profile (typically located at `~/.Rprofile`) when `renv`
#'   is loaded? Consider disabling this if you require extra encapsulation in
#'   your projects; e.g. if your `.Rprofile` attempts to load packages that
#'   you might not install in your projects.
#'   \cr
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

renv_config <- function(name, ..., scope = "config", default = NULL) {

  # validate call
  dots <- eval(substitute(alist(...)))
  if (length(dots))
    stopf("internal error: unexpected arguments supplied to renv_config")

  # check for R option of associated name
  optname <- tolower(name)
  optkey <- paste("renv", scope, optname, sep = ".")
  optval <- getOption(optkey)
  if (!is.null(optval))
    return(optval)

  # check for environment variable
  envname <- gsub(".", "_", toupper(name), fixed = TRUE)
  envkey <- paste("RENV", toupper(scope), envname, sep = "_")
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
