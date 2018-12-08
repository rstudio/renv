renv_load_r_version <- function(config) {
  version <- config$r_version
  if (!version_compatible(version, getRversion())) {
    fmt <- "renv '%s' requested R version '%s' but '%s' is currently being used"
    warningf(fmt, renv_active_renv(), version, getRversion())
  }
}

renv_load_libpaths <- function(config) {

  renv <- file.path(renv_paths_renv(), sprintf("renv-%s", config$renv_version))
  libs <- rev(renv_paths_library(config$r_libs))
  lapply(libs, ensure_directory)

  libpaths <- c(libs, renv, if (config$r_libs_overlay) .libPaths())
  .libPaths(libpaths)

}

renv_load_repos <- function(config) {
  if (!empty(config$r_repos))
    options(repos = config$r_repos)
}

renv_load_report <- function() {
  if (!renv_verbose())
    return()

  renv <- renv_active_renv()
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
  active <- file.path(project, "renv/active")
  if (!file.exists(active)) {
    fmt <- "Project '%s' does not have an active virtual environment"
    msg <- sprintf(fmt, aliased_path(project))
    stop(msg, call. = FALSE)
  }

  dcf <- attempt(read.dcf(active, all = TRUE))
  renv <- dcf$name
  local <- as.logical(dcf$local)

  renv_set_local(local)
  renv

}
