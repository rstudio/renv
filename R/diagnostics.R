
#' Print a diagnostics report
#'
#' Print a diagnostics report, summarizing the state of a project using renv.
#' This report can occasionally be useful when diagnosing issues with renv.
#'
#' @inheritParams renv-params
#'
#' @return This function is normally called for its side effects.
#'
#' @export
diagnostics <- function(project = NULL) {

  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  if (renv_file_type(project, symlinks = FALSE) != "directory") {
    fmt <- "project %s is not a directory"
    stopf(fmt, renv_path_pretty(project))
  }

  renv_scope_options(renv.verbose = TRUE)

  reporters <- list(
    renv_diagnostics_os,
    renv_diagnostics_session,
    renv_diagnostics_project,
    renv_diagnostics_status,
    renv_diagnostics_packages,
    renv_diagnostics_abi,
    renv_diagnostics_profile,
    renv_diagnostics_settings,
    renv_diagnostics_options,
    renv_diagnostics_envvars,
    renv_diagnostics_path,
    renv_diagnostics_cache
  )

  fmt <- "Diagnostics Report [renv %s]"
  title <- sprintf(fmt, renv_metadata_version_friendly())
  lines <- paste(rep.int("=", nchar(title)), collapse = "")
  writef(c(title, lines, ""))

  for (reporter in reporters) {
    tryCatch(reporter(project), error = renv_error_handler)
    writef()
  }

}

renv_diagnostics_os <- function(project) {

  if (renv_platform_linux()) {
    releases <- list.files("/etc", pattern = "-release$", full.names = TRUE)
    for (release in releases) {
      writef(header(release))
      writeLines(readLines(release))
      writef()
    }
  }

}

renv_diagnostics_session <- function(project) {
  writef(header("Session Info"))
  renv_scope_options(width = 80)
  print(sessionInfo())
}

renv_diagnostics_project <- function(project) {
  writef(header("Project"))
  writef("Project path: %s", renv_path_pretty(project))
}

renv_diagnostics_status <- function(project) {
  writef(header("Status"))
  status(project = project)
}

renv_diagnostics_packages <- function(project) {

  writef(header("Packages"))

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
  all <- c(
    names(lockfile$Packages),
    names(libstate$Packages),
    names(recdeps),
    used
  )

  # sort
  all <- csort(unique(all))

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
  data <- data_frame(
    Library    = renv_diagnostics_packages_version(libstate, all),
    Source     = renv_diagnostics_packages_sources(libstate, all),
    Lockfile   = renv_diagnostics_packages_version(lockfile, all),
    Source     = renv_diagnostics_packages_sources(lockfile, all),
    Path       = libcodes,
    Dependency = deps
  )

  # we explicitly want to use rownames here
  row.names(data) <- names(deps)

  # print it out
  renv_scope_options(width = 9000)
  print(data, max = 10000)

  # print library codes
  fmt <- "[%s]: %s"
  writef()
  writef(fmt, format(seq_along(levels(flibpaths))), format(levels(flibpaths)))

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

  lockpath <- renv_lockfile_path(project = project)
  if (!file.exists(lockpath)) {
    writef("This project has not yet been snapshotted: 'renv.lock' does not exist.")
    return(list())
  }

  renv_lockfile_read(lockpath)

}

renv_diagnostics_packages_library <- function(project) {

  library <- renv_paths_library(project = project)
  if (!file.exists(library)) {
    fmt <- "The project library %s does not exist."
    writef(fmt, renv_path_pretty(library))
  }

  snapshot(project = project, lockfile = NULL, type = "all")

}

renv_diagnostics_packages_dependencies <- function(project) {

  renv_dependencies_impl(
    project,
    errors = "reported",
    dev = TRUE
  )

}

renv_diagnostics_abi <- function(project) {

  writef(header("ABI"))
  tryCatch(
    renv_abi_check(),
    error = function(e) {
      writef(conditionMessage(e))
    }
  )

}

renv_diagnostics_profile <- function(project) {

  writef(header("User Profile"))

  userprofile <- "~/.Rprofile"
  if (!file.exists(userprofile))
    return(writef("[no user profile detected]"))

  deps <- renv_dependencies_impl(
    userprofile,
    errors = "reported",
    dev = TRUE
  )

  if (empty(deps))
    return(writef("[no R packages referenced in user profile"))

  renv_scope_options(width = 200)
  print(deps)

}

renv_diagnostics_settings <- function(project) {
  writef(header("Settings"))
  str(renv_settings_get(project))
}

renv_diagnostics_options <- function(project) {

  writef(header("Options"))

  keys <- c(
    "defaultPackages",
    "download.file.method",
    "download.file.extra",
    "install.packages.compile.from.source",
    "pkgType",
    "repos",
    grep("^renv[.]", names(.Options), value = TRUE)
  )

  vals <- .Options[keys]
  names(vals) <- keys

  str(vals)

}

renv_diagnostics_envvars <- function(project) {

  writef(header("Environment Variables"))

  envvars <- convert(as.list(Sys.getenv()), "character")

  useful <- c(
    "R_LIBS_USER", "R_LIBS_SITE", "R_LIBS",
    "HOME", "LANG", "MAKE",
    grep("_proxy", names(envvars), ignore.case = TRUE, value = TRUE),
    grep("^RENV_", names(envvars), value = TRUE)
  )

  matches <- envvars[useful]
  if (empty(matches))
    return(writef("[no renv environment variables available]"))

  names(matches) <- useful
  matches[is.na(matches)] <- "<NA>"
  matches <- matches[order(names(matches))]

  keys <- names(matches)
  vals <- matches
  formatted <- paste(format(keys), vals, sep = " = ")
  writef(formatted)

}

renv_diagnostics_path <- function(project) {
  writef(header("PATH"))
  path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  writef(paste("-", path))
}

renv_diagnostics_cache <- function(project) {

  writef(header("Cache"))

  fmt <- "There are a total of %s installed in the renv cache."
  cachelist <- renv_cache_list()
  writef(fmt, nplural("package", length(cachelist)))
  writef("Cache path: %s", renv_path_pretty(renv_paths_cache()))

}
