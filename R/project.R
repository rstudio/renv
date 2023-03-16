
#' Retrieve the Active Project
#'
#' Retrieve the path to the active project (if any).
#'
#' @param default The value to return when no project is
#'   currently active. Defaults to `NULL`.
#'
#' @export
#'
#' @return The active project directory, as a length-one character vector.
#'
#' @examples
#' \dontrun{
#'
#' # get the currently-active renv project
#' renv::project()
#'
#' }
project <- function(default = NULL) {
  renv_project(default = default)
}

renv_project_get <- function(default = NULL) {

  project <- Sys.getenv("RENV_PROJECT", unset = NA)
  if (is.na(project))
    return(default)

  project

}

renv_project_set <- function(project) {
  Sys.setenv(RENV_PROJECT = project)
}

renv_project_clear <- function() {
  Sys.unsetenv("RENV_PROJECT")
}

renv_project <- function(default = getwd()) {
  renv_project_get(default = default)
}

renv_project_resolve <- function(project = NULL) {
  project <- project %||% renv_project()
  normalizePath(project, winslash = "/", mustWork = FALSE)
}

renv_project_initialized <- function(project) {

  lockfile <- renv_lockfile_path(project)
  if (file.exists(lockfile))
    return(TRUE)

  library <- renv_paths_library(project = project)
  if (file.exists(library))
    return(TRUE)

  FALSE

}

renv_project_type <- function(path) {

  if (!nzchar(path))
    return("unknown")

  path <- renv_path_normalize(path)
  filebacked(
    scope    = "renv_project_type",
    path     = file.path(path, "DESCRIPTION"),
    callback = renv_project_type_impl
  )

}

renv_project_type_impl <- function(path) {

  if (!file.exists(path))
    return("unknown")

  desc <- tryCatch(
    renv_dcf_read(path),
    error = identity
  )

  if (inherits(desc, "error"))
    return("unknown")

  type <- desc$Type
  if (!is.null(type))
    return(tolower(type))

  package <- desc$Package
  if (!is.null(package))
    return("package")

  "unknown"

}

renv_project_remotes <- function(project, fields = NULL) {

  # if this project has a DESCRIPTION file, use it to provide records
  descpath <- file.path(project, "DESCRIPTION")
  if (file.exists(descpath))
    return(renv_project_remotes_description(project, descpath, fields))

  # otherwise, use the set of (non-base) packages used in the project
  deps <- dependencies(
    path     = project,
    progress = FALSE,
    errors   = "ignored",
    dev      = TRUE
  )

  packages <- sort(unique(deps$Package))
  ignored <- c("renv", renv_project_ignored_packages(project))
  setdiff(packages, ignored)

}

renv_project_remotes_description <- function(project, descpath, fields = NULL) {

  # first, parse remotes (if any)
  remotes <- renv_project_remotes_description_remotes(project, descpath)

  # next, find packages mentioned in the DESCRIPTION file
  fields <- fields %||% c("Depends", "Imports", "LinkingTo", "Suggests")
  deps <- renv_dependencies_discover_description(
    path    = descpath,
    fields  = fields,
    project = project
  )

  if (empty(deps))
    return(list())

  # split according to package
  specs <- split(deps, deps$Package)

  # drop ignored specs
  ignored <- renv_project_ignored_packages(project = project)
  specs <- specs[setdiff(names(specs), c("R", ignored))]

  # if any Roxygen fields are included,
  # infer a dependency on roxygen2 and devtools
  desc <- renv_description_read(descpath)
  if (any(grepl("^Roxygen", names(desc)))) {
    for (package in c("devtools", "roxygen2")) {
      if (!package %in% ignored) {
        specs[[package]] <-
          specs[[package]] %||%
          renv_dependencies_list(descpath, package, dev = TRUE)
      }
    }
  }

  # now, try to resolve the packages
  records <- enumerate(specs, function(package, spec) {

    # use remote if supplied
    if (!is.null(remotes[[package]]))
      return(remotes[[package]])

    # check for explicit version requirement
    explicit <- spec[spec$Require == "==", ]
    if (nrow(explicit) == 0)
      return(renv_remotes_resolve(package))

    version <- spec$Version[[1]]
    if (!nzchar(version))
      return(renv_remotes_resolve(package))

    entry <- paste(package, version, sep = "@")
    renv_remotes_resolve(entry)

  })

  # return records
  records

}

renv_project_remotes_description_remotes <- function(project, descpath) {

  desc <- renv_description_read(descpath)

  profile <- renv_profile_get()
  field <- if (is.null(profile))
    "Remotes"
  else
    sprintf("Config/renv/profiles/%s/remotes", profile)

  remotes <- desc[[field]]
  if (is.null(remotes))
    return(list())

  splat <- strsplit(remotes, "[[:space:]]*,[[:space:]]*")[[1]]
  resolved <- lapply(splat, renv_remotes_resolve)
  names(resolved) <- extract_chr(resolved, "Package")
  resolved

}

renv_project_ignored_packages <- function(project) {

  # if we don't have a project, nothing to do
  if (is.null(project))
    return(character())

  # read base set of ignored packages
  ignored <- c(
    settings$ignored.packages(project = project),
    renv_project_ignored_packages_self(project)
  )

  # return collected set of ignored packages
  ignored

}

renv_project_ignored_packages_self <- function(project) {

  # only ignore self in package projects
  if (renv_project_type(project) != "package")
    return(NULL)

  # read current package
  desc <- renv_description_read(project)
  package <- desc[["Package"]]

  # respect user preference if set
  ignore <- getOption("renv.snapshot.ignore.self", default = NULL)
  if (identical(ignore, TRUE))
    return(package)
  else if (identical(ignore, FALSE))
    return(NULL)

  # don't ignore self in golem projets
  golem <- file.path(project, "inst/golem-config.yml")
  if (file.exists(golem))
    return(NULL)

  # hack for renv: don't depend on self
  if (identical(package, "renv"))
    return(NULL)

  # return the package name
  package

}

renv_project_id <- function(project) {

  idpath <- renv_id_path(project = project)
  if (!file.exists(idpath)) {
    id <- renv_id_generate()
    writeLines(id, con = idpath)
  }

  readLines(idpath, n = 1L, warn = FALSE)

}

renv_project_synchronized_check <- function(project = NULL, lockfile = NULL) {

  project  <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_load(project)

  # check for packages referenced in the lockfile which are not installed
  lockpkgs <- names(lockfile$Packages)
  libpkgs <- renv_snapshot_library(
    library = renv_libpaths_all(),
    project = project,
    records = FALSE
  )

  # ignore renv
  lockpkgs <- setdiff(lockpkgs, "renv")
  libpkgs <- setdiff(libpkgs, "renv")

  # check for case where no packages are installed (except renv)
  if (length(lockpkgs) > 1L) {
    ok <- intersect(lockpkgs, basename(libpkgs))
    if (length(ok) == 0L || identical(ok, "renv")) {
      msg <- lines(
        "* This project contains a lockfile, but none of the recorded packages are installed.",
        "* Use `renv::restore()` to restore the project library."
      )
      ewritef(msg)
      return(FALSE)
    }
  }

  # check for case where one or more packages are missing
  missing <- setdiff(lockpkgs, basename(libpkgs))
  if (length(missing)) {
    msg <- lines(
      "* One or more packages recorded in the lockfile are not installed.",
      "* Use `renv::status()` for more details."
    )
    ewritef(msg)
    return(FALSE)
  }

  # otherwise, use status to detect if we're synchronized
  info <- quietly({
    renv_scope_options(renv.verbose = FALSE)
    status(project = project, sources = FALSE)
  })

  if (!identical(info$synchronized, TRUE)) {

    msg <- lines(
      "* The project is currently out-of-sync.",
      "* Use `renv::status()` for more details."
    )

    ewritef(msg)
    return(FALSE)

  }

  TRUE

}

# TODO: this gets really dicey once the user starts configuring where
# renv places its project-local state ...
renv_project_find <- function(project = NULL) {

  project <- project %||% getwd()

  anchors <- c("renv.lock", "renv/activate.R")
  resolved <- renv_file_find(project, function(parent) {
    for (anchor in anchors)
      if (file.exists(file.path(parent, anchor)))
        return(parent)
  })

  if (is.null(resolved)) {
    fmt <- "couldn't resolve renv project associated with path %s"
    stopf(fmt, renv_path_pretty(project))
  }

  resolved

}

renv_project_lock <- function(project = NULL) {

  if (!config$locking.enabled())
    return()

  path <- getOption("renv.project.path")
  if (!identical(project, path))
    return()

  project <- renv_project_resolve(project)
  path <- file.path(project, "renv/lock")
  ensure_parent_directory(path)
  renv_scope_lock(path, envir = parent.frame())

}

renv_project_active <- function() {
  !is.null(getOption("renv.project.path"))
}
