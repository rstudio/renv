
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
}

renv_envvars_normalize <- function() {

  Sys.setenv(R_LIBS_SITE = .expand_R_libs_env_var(Sys.getenv("R_LIBS_SITE")))
  Sys.setenv(R_LIBS_USER = .expand_R_libs_env_var(Sys.getenv("R_LIBS_USER")))

  keys <- c(
    "RENV_PATHS_ROOT",
    "RENV_PATHS_LIBRARY",
    "RENV_PATHS_LIBRARY_ROOT",
    "RENV_PATHS_LIBRARY_STAGING",
    "RENV_PATHS_LOCAL",
    "RENV_PATHS_CELLAR",
    "RENV_PATHS_SOURCE",
    "RENV_PATHS_BINARY",
    "RENV_PATHS_CACHE",
    "RENV_PATHS_RTOOLS",
    "RENV_PATHS_EXTSOFT",
    "RENV_PATHS_MRAN"
  )

  envvars <- as.list(keep(Sys.getenv(), keys))
  if (empty(envvars))
    return()

  args <- lapply(envvars, renv_path_normalize)
  do.call(Sys.setenv, args)

}
