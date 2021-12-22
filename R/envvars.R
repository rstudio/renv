
renv_envvars_list <- function() {
  c(
    "R_PROFILE", "R_PROFILE_USER",
    "R_ENVIRON", "R_ENVIRON_USER",
    "R_LIBS_USER", "R_LIBS_SITE", "R_LIBS"
  )
}

renv_envvars_save <- function() {

  # save the common set of environment variables
  keys <- renv_envvars_list()
  vals <- Sys.getenv(keys, unset = "<NA>")

  # check for defaults that have already been set
  defkeys <- paste("RENV_DEFAULT", keys, sep = "_")
  defvals <- Sys.getenv(defkeys, unset = NA)
  if (any(!is.na(defvals)))
    return(FALSE)

  # prepare defaults
  env <- vals
  names(env) <- defkeys
  do.call(Sys.setenv, as.list(env))

  TRUE

}

renv_envvars_restore <- function() {

  # read defaults
  keys <- renv_envvars_list()
  defkeys <- paste("RENV_DEFAULT", renv_envvars_list(), sep = "_")
  defvals <- Sys.getenv(defkeys, unset = "<NA>")

  # remove previously-unset environment variables
  missing <- defvals == "<NA>"
  Sys.unsetenv(keys[missing])

  # restore old values for envvars
  existing <- as.list(defvals[!missing])
  if (length(existing)) {
    names(existing) <- sub("^RENV_DEFAULT_", "", names(existing))
    do.call(Sys.setenv, existing)
  }

  # remove saved RENV_DEFAULT values
  Sys.unsetenv(defkeys)
  TRUE

}

renv_envvars_init <- function() {

  renv_envvars_normalize()

  if (renv_platform_macos()) {

    # set SDKROOT so older R installations can find command line tools
    sdk <- "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
    sdkroot <- Sys.getenv("SDKROOT", unset = NA)
    if (is.na(sdkroot) && file.exists(sdk))
      Sys.setenv(SDKROOT = sdk)

  }

}

renv_envvars_normalize <- function() {

  Sys.setenv(R_LIBS_SITE = .expand_R_libs_env_var(Sys.getenv("R_LIBS_SITE")))
  Sys.setenv(R_LIBS_USER = .expand_R_libs_env_var(Sys.getenv("R_LIBS_USER")))

  envvars <- as.list(Sys.getenv())

  keys <- grep("^RENV_PATHS_", names(envvars), value = TRUE)
  ignore <- c("RENV_PATHS_PREFIX", "RENV_PATHS_PREFIX_AUTO", "RENV_PATHS_RENV")
  keys <- setdiff(keys, ignore)
  if (empty(keys))
    return(character())

  args <- lapply(envvars[keys], renv_path_normalize)
  do.call(Sys.setenv, args)

}
