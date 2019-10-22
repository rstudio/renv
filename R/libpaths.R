
`_renv_libpaths` <- new.env(parent = emptyenv())

# NOTE: if sandboxing is used then these symbols will be clobbered;
# save them so we can properly restore them later if so required
renv_libpaths_init <- function() {
  assign(".Library",      .Library,      envir = `_renv_libpaths`)
  assign(".Library.site", .Library.site, envir = `_renv_libpaths`)
}

renv_libpaths_default <- function() {
  .libPaths()[1]
}

renv_libpaths_all <- function() {
  .libPaths()
}

renv_libpaths_system <- function() {
  get(".Library", envir = `_renv_libpaths`)
}

renv_libpaths_site <- function() {
  get(".Library.site", envir = `_renv_libpaths`)
}

renv_libpaths_external <- function(project) {

  projlib <- settings$external.libraries(project = project)

  conflib <- renv_config("external.libraries", default = character())
  if (is.function(conflib))
    conflib <- conflib(project = project)

  .expand_R_libs_env_var(c(projlib, conflib))

}

renv_libpaths_set <- function(libpaths) {
  oldlibpaths <- .libPaths()
  .libPaths(libpaths)
  oldlibpaths
}

# NOTE: may return more than one library path!
renv_libpaths_user <- function() {

  # if renv is active, the user library will be saved
  envvars <- c("RENV_DEFAULT_R_LIBS_USER", "R_LIBS_USER")
  for (envvar in envvars) {

    value <- Sys.getenv(envvar, unset = NA)
    if (is.na(value) || value == "<NA>" || !nzchar(value))
      next

    parts <- strsplit(value, .Platform$path.sep, fixed = TRUE)[[1]]
    return(parts)

  }

  # otherwise, default to active library (shouldn't happen but best be safe)
  renv_libpaths_default()

}

renv_libpaths_activate <- function(project) {

  projlib <- renv_paths_library(project = project)
  extlib <- renv_libpaths_external(project = project)
  userlib <- if (renv_config("user.library", default = FALSE))
    renv_libpaths_user()

  libpaths <- c(projlib, extlib, userlib)

  lapply(libpaths, ensure_directory)
  oldlibpaths <- .libPaths()
  .libPaths(libpaths)
  oldlibpaths

}

renv_libpaths_save <- function() {
  libpaths <- renv_global_get("default.libpaths")
  if (is.null(libpaths))
    renv_global_set("default.libpaths", libpaths)
}

renv_libpaths_restore <- function() {
  libpaths <- renv_global_get("default.libpaths")
  if (!is.null(libpaths)) {
    renv_global_clear("default.libpaths")
    .libPaths(libpaths)
  }
}
