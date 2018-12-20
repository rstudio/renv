
renv_load_r_version <- function(manifest) {
  version <- manifest$R$Version
  if (version_compare(version, getRversion()) != 0) {
    fmt <- "Environment '%s' requested R version '%s' but '%s' is currently being used"
    warningf(fmt, renv_active_environment(), version, getRversion())
  }
}

renv_load_libpaths <- function(manifest) {

  libs <- rev(renv_paths_library(manifest$R$Libraries))
  lapply(libs, ensure_directory)

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

  renv <- renv_active_environment()
  local <- renv_local()
  fmt <- if (local)
    "* Local virtual environment '%s' loaded."
  else
    "* Virtual environment '%s' loaded."

  messagef(fmt, basename(renv))
  message()

  paths <- paste("-", shQuote(.libPaths()), collapse = "\n")
  message("* Using library paths:")
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

  dcf <- catch(renv_dcf_read(state, all = TRUE))
  if (inherits(dcf, "error"))
    return(dcf)

  renv_set_local(as.logical(dcf$Local))
  dcf$Environment

}
