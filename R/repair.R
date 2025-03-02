
#' Repair a project
#'
#' Use `repair()` to recover from some common issues that can occur with
#' a project. Currently, two operations are performed:
#'
#' 1. Packages with broken symlinks into the cache will be re-installed.
#'
#' 2. Packages that were installed from sources, but appear to be from
#'    an remote source (e.g. GitHub), will have their `DESCRIPTION` files
#'    updated to record that remote source explicitly.
#'
#' @inheritParams renv-params
#'
#' @param lockfile The path to a lockfile (if any). When available, renv
#'   will use the lockfile when attempting to infer the remote associated
#'   with the inaccessible version of each missing package. When `NULL`
#'   (the default), the project lockfile will be used.
#'
#' @export
repair <- function(library  = NULL,
                   lockfile = NULL,
                   project  = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  libpaths <- renv_path_normalize(library %||% renv_libpaths_all())
  library <- libpaths[[1L]]

  writef(header("Library cache links"))
  renv_repair_links(library, lockfile, project)
  writef()

  writef(header("Package sources"))
  renv_repair_sources(library, lockfile, project)
  writef()

  invisible()
}

renv_repair_links <- function(library, lockfile, project) {


  # figure out which library paths (junction points?) appear to be broken
  paths <- list.files(library, full.names = TRUE)
  broken <- renv_file_broken(paths)
  packages <- basename(paths[broken])
  if (empty(packages)) {
    writef("- No issues found with the project library's cache links.")
    return(invisible(packages))
  }

  # try to find records for these packages in the lockfile
  # TODO: what if one of the requested packages isn't in the lockfile?
  lockfile <- lockfile %||% renv_lockfile_load(project = project)
  records <- renv_repair_records(packages, lockfile, project)

  # install these records
  install(
    packages = records,
    library  = library,
    project  = project
  )

}

renv_repair_records <- function(packages, lockfile, project) {
  map(packages, function(package) {
    lockfile$Packages[[package]] %||% package
  })
}

renv_repair_sources <- function(library, lockfile, project) {

  # get package description files
  db <- installed_packages(lib.loc = library, priority = NA_character_)
  descpaths <- with(db, file.path(LibPath, Package, "DESCRIPTION"))
  dcfs <- map(descpaths, renv_description_read)
  names(dcfs) <- map_chr(dcfs, `[[`, "Package")

  # try to infer sources as necessary
  inferred <- map(dcfs, renv_repair_sources_infer)
  inferred <- filter(inferred, Negate(is.null))
  if (length(inferred) == 0L) {
    writef("- All installed packages appear to be from a known source.")
    return(TRUE)
  }

  # ask used
  renv_scope_options(renv.verbose = TRUE)
  bulletin(
    c(
      "The following package(s) do not have an explicitly-declared remote source.",
      "However, renv was available to infer remote sources from their DESCRIPTION file."
    ),
    sprintf("%s  [%s]", format(names(inferred)), inferred),
    "`renv::restore()` may fail for packages without an explicitly-declared remote source."
  )

  choice <- menu(

    choices =  c(
      update = "Let renv infer the remote sources for these packages.",
      cancel = "Do nothing and resolve the situation another way."
    ),

    title = "What would you like to do?"

  )

  cancel_if(identical(choice, "cancel"))

  enumerate(inferred, function(package, remote) {
    record <- renv_remotes_resolve(remote)
    record[["RemoteSha"]] <- NULL
    renv_package_augment(file.path(library, package), record)
  })

  n <- length(inferred)
  writef("- Updated %i package DESCRIPTION %s.", n, nplural("file", n))

  TRUE

}

renv_repair_sources_infer <- function(dcf) {

  # if this package appears to have a declared remote, use as-is
  for (field in c("RemoteType", "Repository", "biocViews"))
    if (!is.null(dcf[[field]]))
      return(NULL)

  # ok, this is a package installed from sources that "looks" like
  # the development version of a package; try to guess its remote
  guess <- function(pattern, field) {
    urls <- strsplit(dcf[[field]] %||% "", "\\s*,\\s*")[[1L]]
    for (url in urls) {
      matches <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1L]]
      if (length(matches) == 3L)
        return(paste(matches[[2L]], matches[[3L]], sep = "/"))
    }
  }

  # first, check bug reports
  remote <- guess("^https://(?:www\\.)?github\\.com/([^/]+)/([^/]+)/issues$", "BugReports")
  if (!is.null(remote))
    return(remote)

  # next, check the URL field
  remote <- guess("^https://(?:www\\.)?github\\.com/([^/]+)/([^/]+)", "URL")
  if (!is.null(remote))
    return(remote)

}
