
renv_load_r_version <- function(manifest) {
  version <- manifest$R$Version
  if (version_compare(version, getRversion()) != 0) {
    fmt <- "Environment '%s' requested R version '%s' but '%s' is currently being used"
    warningf(fmt, renv_active_environment_get(), version, getRversion())
  }
}

renv_load_libpaths <- function(manifest) {

  libraries <- manifest$R$Libraries

  libs <- NULL
  if (length(libraries)) {
    libs <- rev(renv_paths_library(manifest$R$Libraries))
    lapply(libs, ensure_directory)
  }

  libpaths <- c(libs, if (manifest$R$Overlay) .libPaths())

  Sys.setenv(R_LIBS_USER = paste(libpaths, collapse = .Platform$path.sep))
  .libPaths(libpaths)

}

renv_load_repos <- function(manifest) {
  options(repos = manifest$R$Repositories)
}

renv_load_finish <- function() {

  if (!renv_verbose())
    return()

  renv <- renv_active_environment_get()
  local <- renv_active_local_get()
  paths <- paste("-", shQuote(.libPaths()), collapse = "\n")

  fmt <- "%s environment '%s' loaded. Using library paths:"
  messagef(fmt, if (local) "Local virtual" else "Virtual", basename(renv))
  message(paths)

}

renv_load_project <- function(project) {

  # when 'renv' is not specified, use the active virtual environment associated
  # with the project (if any)
  state <- file.path(project, "renv/renv.dcf")
  if (!file.exists(state)) {
    fmt <- "Project '%s' does not have an active virtual environment."
    msg <- sprintf(fmt, aliased_path(project))
    stop(msg, call. = FALSE)
  }

  dcf <- catch(renv_dcf_read(state))
  if (inherits(dcf, "error"))
    return(dcf)

  renv_active_local_set(as.logical(dcf$Local))
  dcf$Environment

}
