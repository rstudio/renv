
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
    renv_diagnostics_packages,
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
    tryCatch(reporter(project), error = renv_error_handler)
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

renv_diagnostics_packages <- function(project) {

  vwritef(header("Packages"))

  # collect state of lockfile, library, dependencies
  lockfile <- renv_diagnostics_packages_lockfile(project)
  libstate <- renv_diagnostics_packages_library(project)
  used <- unique(renv_diagnostics_packages_dependencies(project)$Package)

  # collect recursive package dependencies
  recdeps <- renv_package_dependencies(
    packages = used,
    project  = project
  )

  # bundle together
  all <- c(names(lockfile$Packages), names(libstate$Packages), names(recdeps))

  # sort
  renv_scope_locale(category = "LC_COLLATE", locale = "C")
  all <- sort(unique(all))

  # check which packages are direct, indirect requirements
  deps <- rep.int(NA_character_, length(all))
  names(deps) <- all
  deps[names(recdeps)] <- "indirect"
  deps[used] <- "direct"

  # build libpaths for installed packages
  libpaths <- dirname(map_chr(all, renv_package_find))

  # use short form
  flibpaths <- factor(libpaths, levels = .libPaths())

  # construct integer codes (to be reported in data output)
  libcodes <- as.integer(flibpaths)
  libcodes[!is.na(libcodes)] <- sprintf("[%i]", libcodes[!is.na(libcodes)])

  # add in packages in library
  data <- data.frame(

    Library    = renv_diagnostics_packages_version(libstate, all),
    Source     = renv_diagnostics_packages_sources(libstate, all),
    Lockfile   = renv_diagnostics_packages_version(lockfile, all),
    Source     = renv_diagnostics_packages_sources(lockfile, all),
    Path       = libcodes,
    Dependency = deps,

    stringsAsFactors = FALSE,
    check.names      = FALSE

  )

  # print it out
  renv_scope_options(width = 9000)
  print(data)

  # print library codes
  fmt <- "[%s]: %s"
  vwritef()
  vwritef(fmt, format(seq_along(levels(flibpaths))), format(levels(flibpaths)))

}

renv_diagnostics_packages_version <- function(lockfile, all) {

  data <- rep.int(NA_character_, length(all))
  names(data) <- all

  formatted <- map_chr(lockfile$Packages, `[[`, "Version")
  data[names(formatted)] <- formatted

  data

}

renv_diagnostics_packages_sources <- function(lockfile, all) {

  data <- rep.int(NA_character_, length(all))
  names(data) <- all

  sources <- map_chr(lockfile$Packages, function(record) {
    record$Repository %||% record$Source %||% "<unknown>"
  })

  data[names(sources)] <- sources
  data

}

renv_diagnostics_packages_lockfile <- function(project) {

  lockpath <- file.path(project, "renv.lock")
  if (!file.exists(lockpath)) {
    vwritef("This project has not yet been snapshotted: 'renv.lock' does not exist.")
    return(list())
  }

  renv_lockfile_read(lockpath)

}

renv_diagnostics_packages_library <- function(project) {

  library <- renv_paths_library(project = project)
  if (!file.exists(library)) {
    fmt <- "The project library %s does not exist."
    vwritef(fmt, renv_path_pretty(library))
  }

  snapshot(project = project, lockfile = NULL, type = "all")

}

renv_diagnostics_packages_dependencies <- function(project) {

  dependencies(project,
               progress = FALSE,
               errors = "reported",
               dev = TRUE)

}

renv_diagnostics_profile <- function(project) {

  vwritef(header("User Profile"))

  userprofile <- "~/.Rprofile"
  if (!file.exists(userprofile))
    return(vwritef("[no user profile detected]"))

  deps <- dependencies(userprofile,
                       progress = FALSE,
                       errors = "reported",
                       dev = TRUE)

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
