
# The path to the currently-loaded project, if any.
# NULL when no project is currently loaded.
the$project_path <- NULL

# Flag indicating whether we're checking if the project is synchronized.
the$project_synchronized_check_running <- FALSE

#' Retrieve the active project
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
  renv_project_get(default = default)
}

renv_project_get <- function(default = NULL) {
  the$project_path %||% default
}

# NOTE: RENV_PROJECT kept for backwards compatibility with RStudio
renv_project_set <- function(project) {
  the$project_path <- project
  Sys.setenv(RENV_PROJECT = project)
}

# NOTE: 'RENV_PROJECT' kept for backwards compatibility with RStudio
renv_project_clear <- function() {
  the$project_path <- NULL
  Sys.unsetenv("RENV_PROJECT")
}

renv_project_resolve <- function(project = NULL, default = getwd()) {
  project <- project %||% renv_project_get(default = default)
  renv_path_normalize(project)
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
    context  = "renv_project_type",
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

renv_project_remotes <- function(project, fields = NULL, resolve = FALSE) {

  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath))
    return(NULL)

  # first, parse remotes (if any)
  remotes <- renv_description_remotes(descpath)

  # next, find packages mentioned in the DESCRIPTION file
  deps <- renv_dependencies_discover_description(
    path    = descpath,
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

    # return a function here, so we can lazily resolve these as needed
    # https://github.com/rstudio/renv/issues/1755
    function() {

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

    }

  })

  if (resolve) map(records, resolve) else records

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

# TODO: this gets really dicey once the user starts configuring where
# renv places its project-local state ...
renv_project_find <- function(path = NULL) {

  path <- path %||% getwd()

  anchors <- c("renv.lock", "renv/activate.R")
  resolved <- renv_file_find(path, function(parent) {
    for (anchor in anchors)
      if (file.exists(file.path(parent, anchor)))
        return(parent)
  })

  if (is.null(resolved)) {
    fmt <- "couldn't resolve renv project associated with path %s"
    stopf(fmt, renv_path_pretty(path))
  }

  resolved

}

renv_project_lock <- function(project = NULL) {

  if (!config$locking.enabled())
    return()

  path <- the$project_path
  if (!identical(project, path))
    return()

  project <- renv_project_resolve(project)
  path <- file.path(project, "renv/lock")
  ensure_parent_directory(path)
  renv_scope_lock(path, scope = parent.frame())

}

renv_project_loaded <- function(project) {
  !is.null(project) && identical(project, the$project_path)
}
