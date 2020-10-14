
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

renv_project <- function(default = getwd()) {
  project <- Sys.getenv("RENV_PROJECT", unset = NA)
  if (is.na(project))
    return(default)
  project
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

  descpath <- file.path(path, "DESCRIPTION")
  if (!file.exists(descpath))
    return("unknown")

  renv_description_type(descpath)

}

renv_project_records <- function(project) {

  # if this project has a DESCRIPTION file, use it to provide records
  descpath <- file.path(project, "DESCRIPTION")
  if (file.exists(descpath))
    return(renv_project_records_description(project, descpath))

}

renv_project_records_description <- function(project, descpath) {

  # first, parse remotes (if any)
  remotes <- renv_project_records_description_remotes(project, descpath)

  # next, find packages mentioned in the DESCRIPTION file
  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")
  deps <- renv_dependencies_discover_description(descpath, fields)
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

renv_project_records_description_remotes <- function(project, descpath) {

  desc <- renv_description_read(descpath)

  remotes <- desc$Remotes
  if (is.null(desc$Remotes))
    return(list())

  splat <- strsplit(remotes, "\\s*,\\s*")[[1]]
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
  diff <- renv_lockfile_diff_packages(lockfile, libstate)

  # we only want to report cases where a package version
  # has changed, or the lockfile references a package that
  # is not currently installed in the project library
  diff <- diff[diff != "install"]
  if (length(diff)) {
    msg <- "* The project may be out of sync -- use `renv::status()` for more details."
    ewritef(msg)
  }

}
