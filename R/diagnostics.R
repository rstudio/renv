
#' Print a Diagnostics Report
#'
#' Print a diagnostics report, summarizing the state of a project using `renv`.
#' This report can occasionally be useful when diagnosing issues with `renv`.
#'
#' @inheritParams renv-params
#'
#' @return This function is normally called for its side effects.
#'
#' @export
diagnostics <- function(project = NULL) {

  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  if (renv_file_type(project, symlinks = FALSE) != "directory") {
    fmt <- "project %s is not a directory"
    stopf(fmt, renv_path_pretty(project))
  }

  renv_scope_options(renv.verbose = TRUE)

  reporters <- list(
    renv_diagnostics_session,
    renv_diagnostics_project,
    renv_diagnostics_status,
    renv_diagnostics_lockfile,
    renv_diagnostics_library,
    renv_diagnostics_dependencies,
    renv_diagnostics_profile,
    renv_diagnostics_settings,
    renv_diagnostics_options,
    renv_diagnostics_envvars,
    renv_diagnostics_path,
    renv_diagnostics_cache
  )

  fmt <- "Diagnostics Report -- renv [%s]"
  title <- sprintf(fmt, renv_package_version("renv"))
  lines <- paste(rep.int("=", nchar(title)), collapse = "")
  vwritef(c(title, lines, ""))

  for (reporter in reporters) {
    catch(reporter(project))
    vwritef()
  }

}

renv_diagnostics_session <- function(project) {
  vwritef(header("Session Info"))
  renv_scope_options(width = 80)
  print(sessionInfo())
}

renv_diagnostics_project <- function(project) {
  vwritef(header("Project"))
  vwritef("Project path: %s", renv_path_pretty(project))
}

renv_diagnostics_status <- function(project) {
  vwritef(header("Status"))
  status(project = project)
}

renv_diagnostics_lockfile <- function(project) {

  vwritef(header("Lockfile"))

  lockpath <- file.path(project, "renv.lock")
  if (!file.exists(lockpath)) {
    vwritef("This project has not yet been snapshotted: 'renv.lock' does not exist.")
    return()
  }

  lockfile <- renv_lockfile_read(lockpath)
  records <- renv_records(lockfile)
  vwritef("There are %i package(s) recorded in the lockfile.", length(records))
  renv_pretty_print_records(records)

}

renv_diagnostics_library <- function(project) {

  vwritef(header("Library"))

  library <- renv_paths_library(project = project)
  if (!file.exists(library)) {
    fmt <- "The project library %s does not exist."
    return(vwritef(fmt, renv_path_pretty(library)))
  }

  lockfile <- snapshot(project = project,
                       library = library,
                       lockfile = NULL,
                       type = "simple")

  records <- renv_records(lockfile)
  vwritef("The are %i package(s) installed in the project library.", length(records))
  vwritef("Library path: %s", renv_path_pretty(library))

}

renv_diagnostics_dependencies <- function(project) {

  vwritef(header("Dependencies"))

  deps <- dependencies(project, quiet = TRUE, dev = TRUE)
  if (empty(deps))
    return(vwritef("[no usages of R packages discovered in this project]"))

  renv_scope_options(width = 200)
  print(deps)

}

renv_diagnostics_profile <- function(project) {

  vwritef(header("User Profile"))

  userprofile <- "~/.Rprofile"
  if (!file.exists(userprofile))
    return(vwritef("[no user profile detected]"))

  deps <- dependencies(userprofile, quiet = TRUE, dev = TRUE)
  if (empty(deps))
    return(vwritef("[no R packages referenced in user profile"))

  renv_scope_options(width = 200)
  print(deps)

}

renv_diagnostics_settings <- function(project) {
  vwritef(header("Settings"))
  str(renv_settings_get(project))
}

renv_diagnostics_options <- function(project) {
  vwritef(header("Options"))
  matches <- grep("^renv[.]", names(.Options))
  str(.Options[matches])
}

renv_diagnostics_envvars <- function(project) {

  vwritef(header("Environment Variables"))

  envvars <- convert(as.list(Sys.getenv()), "character")

  useful <- c(
    "R_LIBS_USER", "R_LIBS_SITE", "R_LIBS",
    "HOME", "LANG",
    grep("^RENV_", names(envvars), value = TRUE)
  )

  matches <- envvars[useful]
  if (empty(matches))
    return(vwritef("[no renv environment variables available]"))

  names(matches) <- useful
  matches[is.na(matches)] <- "<NA>"
  matches <- matches[order(names(matches))]

  keys <- names(matches)
  vals <- matches
  formatted <- paste(format(keys), vals, sep = " = ")
  vwritef(formatted)

}

renv_diagnostics_path <- function(project) {
  vwritef(header("PATH"))
  path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  vwritef(paste("-", path))
}

renv_diagnostics_cache <- function(project) {

  vwritef(header("Cache"))

  fmt <- "There are a total of %i package(s) installed in the renv cache."
  cachelist <- renv_cache_list()
  vwritef(fmt, length(cachelist))
  vwritef("Cache path: %s", renv_path_pretty(renv_paths_cache()))

}
