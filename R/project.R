
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
  renv_bootstrap_project_type(path)
}

renv_project_remotes <- function(project) {

  # if this project has a DESCRIPTION file, use it to provide records
  descpath <- file.path(project, "DESCRIPTION")
  if (file.exists(descpath))
    return(renv_project_remotes_description(project, descpath))

  # otherwise, use the set of (non-base) packages used in the project
  deps <- dependencies(
    path     = project,
    progress = FALSE,
    errors   = "ignored",
    dev      = TRUE
  )

  packages <- sort(unique(deps$Package))
  setdiff(packages, renv_packages_base())

}

renv_project_remotes_description <- function(project, descpath) {

  # first, parse remotes (if any)
  remotes <- renv_project_remotes_description_remotes(project, descpath)

  # next, find packages mentioned in the DESCRIPTION file
  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")
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
      specs[[package]] <-
        specs[[package]] %||%
        renv_dependencies_list(descpath, package, dev = TRUE)
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

  # if there are no packages (other than renv) available in the project library,
  # but there are multiple packages referenced in the lockfile,
  # then instruct the user that they may need to run restore
  libpath <- renv_paths_library(project = project)
  packages <- list.files(libpath)

  needsrestore <-
    empty(setdiff(packages, "renv")) &&
    length(lockfile$Packages) > 1

  if (needsrestore) {

    msg <- lines(
      "* The project library is out of sync with the lockfile.",
      "* Use `renv::restore()` to install packages recorded in the lockfile."
    )

    ewritef(msg)
    return(FALSE)
  }

  # perform a lightweight comparison between the project
  # library and lockfile, and report if there may be
  # any conflicts
  library <- renv_libpaths_all()

  # don't validate lockfile state in this scope; just try
  # to read and use it as-is
  renv_scope_options(renv.config.snapshot.validate = FALSE)

  # read current state of project library
  libstate <- snapshot(
    project  = project,
    library  = library,
    lockfile = NULL,
    type     = "all",
    force    = TRUE
  )

  # compare with the current lockfile
  diff <- renv_lockfile_diff_packages(libstate, lockfile)

  # we only want to report cases where a package version
  # has changed, or the lockfile references a package that
  # is not currently installed in the project library
  diff <- diff[diff != "remove"]
  if (!empty(diff)) {
    msg <- "* The project may be out of sync -- use `renv::status()` for more details."
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
