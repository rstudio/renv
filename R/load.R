
renv_load_r_version <- function(manifest) {
  version <- manifest$R$Version
  if (version_compare(version, getRversion()) != 0) {
    fmt <- "Environment '%s' requested R version '%s' but '%s' is currently being used"
    warningf(fmt, renv_state$environment(), version, getRversion())
  }
}

renv_load_libpaths <- function(manifest) {

  libraries <- manifest$R$Library

  libs <- character()
  if (length(libraries)) {
    libs <- rev(renv_paths_library(manifest$R$Library))
    lapply(libs, ensure_directory)
  }

  libpaths <- c(libs, if (manifest$R$Overlay) .libPaths())

  # TODO: if libpaths is empty, should we always use a temporary library?
  # or just use none? or some 'default' library path?

  Sys.setenv(R_LIBS_USER = paste(libpaths, collapse = .Platform$path.sep))
  .libPaths(libpaths)

}

renv_load_repos <- function(manifest) {
  options(repos = manifest$R$Repositories)
}

renv_load_finish <- function() {

  if (!renv_verbose())
    return()

  renv <- renv_state$environment()
  local <- renv_state$local()
  paths <- paste("-", shQuote(.libPaths()), collapse = "\n")

  fmt <- "%s environment '%s' loaded. Using library paths:"
  messagef(fmt, if (local) "Local virtual" else "Virtual", basename(renv))
  message(paths)

}

renv_load_project <- function(project) {
  activate <- renv_activate_read(project)
  renv_state$local(as.logical(activate$Local))
  activate$Environment
}
