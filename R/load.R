
renv_load_r_version <- function(spec) {
  version <- spec$R$Version
  if (version_compare(version, getRversion()) != 0) {
    fmt <- "Environment '%s' requested R version '%s' but '%s' is currently being used"
    warningf(fmt, renv_active_renv(), version, getRversion())
  }
}

renv_load_libpaths <- function(spec) {

  libs <- rev(renv_paths_library(spec$R$Libraries))
  lapply(libs, ensure_directory)

  libpaths <- c(libs, if (spec$R$Overlay) .libPaths())
  .libPaths(libpaths)

}

remv_load_callbacks <- function(spec) {

  addTaskCallback(function(...) {
    if (identical(.libPaths(), spec))
      return(FALSE)

    renv_load_libpaths(spec)
    return(FALSE)
  })

}

renv_load_repos <- function(spec) {
  options(repos = spec$R$Repositories)
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
  state <- file.path(project, "renv/renv.dcf")
  if (!file.exists(state)) {
    fmt <- "Project '%s' does not have an active virtual environment."
    msg <- sprintf(fmt, aliased_path(project))
    stop(msg, call. = FALSE)
  }

  dcf <- tryCatch(read.dcf(state, all = TRUE), error = identity)
  if (inherits(dcf, "error"))
    return(dcf)

  renv_set_local(as.logical(dcf$Local))
  dcf$Environment

}
