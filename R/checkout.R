
#' Checkout a repository
#'
#' `renv::checkout()` can be used to retrieve the latest-availabe packages from
#' a (set of) package repositories.
#'
#' `renv::checkout()` is most useful with services like the Posit's
#' [Package Manager](https://packagemanager.rstudio.com/), as it
#' can be used to switch between different repository snapshots within an
#' renv project. In this way, you can upgrade (or downgrade) all of the
#' packages used in a particular renv project to the package versions
#' provided by a particular snapshot.
#'
#' If your library contains packages installed from other remote sources (e.g.
#' GitHub), but a version of a package of the same name is provided by the
#' repositories being checked out, then please be aware that the package will be
#' replaced with the version provided by the requested repositories. This could
#' be a concern if your project uses \R packages from GitHub whose name matches
#' that of an existing CRAN package, but is otherwise unrelated to the package
#' on CRAN.
#'
#' @inheritParams renv-params
#'
#' @param repos The \R package repositories to use.
#'
#' @param packages The packages to be installed. When `NULL` (the default),
#'   all packages currently used in the project will be installed, as
#'   determined by [renv::dependencies()]. The recursive dependencies of these
#'   packages will be included as well.
#'
#' @param date The snapshot date to use. When set, the associated snapshot as
#'   available from the Posit's public
#'   [Package Manager](https://packagemanager.rstudio.com/) instance will be
#'   used. Ignored if `repos` is non-`NULL`.
#'
#' @param actions The action(s) to perform with the requested repositories.
#'   This can either be "snapshot", in which `renv` will generate a lockfile
#'   based on the latest versions of the packages available from `repos`, or
#'   "restore" if you'd like to install those packages. You can use
#'   `c("snapshot", "restore")` if you'd like to generate a lockfile and
#'   install those packages in the same step.
#'
#' @examples
#' \dontrun{
#'
#' # check out packages from PPM using the date '2023-01-02'
#' renv::checkout(date = "2023-01-02")
#'
#' # alternatively, supply the full repository path
#' renv::checkout(repos = "https://packagemanager.rstudio.com/cran/2023-01-02")
#'
#' # only check out some subset of packages (and their recursive dependencies)
#' renv::checkout(packages = "dplyr", date = "2023-01-02")
#'
#' }
#' @export
checkout <- function(repos = NULL,
                     ...,
                     packages = NULL,
                     date     = NULL,
                     clean    = FALSE,
                     actions  = "restore",
                     project  = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project  <- renv_project_resolve(project)
  renv_project_lock(project = project)

  # set new repositories
  repos <- repos %||% renv_checkout_repos(date)
  options(repos = repos)

  # TODO: Activate Bioconductor if it appears to be used by this project

  # select packages to install
  packages <- packages %||% renv_checkout_packages(project = project)

  # get the associated remotes for these packages
  remotes <- renv_checkout_remotes(packages, project)

  # parse these into package records
  records <- map(remotes, renv_remotes_resolve)

  # create a lockfile matching this request
  lockfile <- renv_lockfile_init(project)
  lockfile$Packages <- records

  # perform requested actions
  for (action in actions) {
    case(
      action == "snapshot" ~ renv_lockfile_write(lockfile, file = renv_lockfile_path(project)),
      action == "restore"  ~ restore(lockfile = lockfile, clean = clean),
      ~ stopf("unrecognized action '%s'")
    )
  }

  invisible(lockfile)

}

renv_checkout_packages <- function(project) {
  renv_dependencies_impl(
    project,
    field = "Package",
    dev = TRUE
  )
}

renv_checkout_remotes <- function(packages, project) {

  # get available packages
  dbs <- available_packages(type = "source")
  if (is.null(dbs))
    stop("no package repositories are available")

  # flatten so we only see the latest version of a package
  db <- renv_available_packages_flatten(dbs)

  # keep only packages which appear to be available in the repositories
  packages <- intersect(packages, db$Package)

  # remove ignored packages -- note we intentionally do this before
  # computing recursive dependencies as we don't want to allow users
  # to ignore a recursive dependency of a required package
  ignored <- c("renv", renv_project_ignored_packages(project))
  packages <- setdiff(packages, ignored)

  # compute recursive dependencies for these packages
  renv_checkout_recdeps(packages, db)

}

renv_checkout_recdeps <- function(packages, db) {

  # initialize environment (will map package names to discovered remotes)
  envir <- new.env(parent = emptyenv())

  # set R to NA since it's a common non-package 'dependency' for packages
  envir$R <- NA

  # iterate through dependencies
  for (package in packages)
    renv_checkout_recdeps_impl(package, db, envir)

  # get list of discovered dependencies
  recdeps <- as.list.environment(envir, all.names = TRUE)

  # drop any NA values
  recdeps <- filter(recdeps, Negate(is.na))

  # return sorted vector
  recdeps[csort(names(recdeps))]

}

renv_checkout_recdeps_impl <- function(package, db, envir) {

  # check if we've already visited this package
  if (!is.null(envir[[package]]))
    return()

  # get entry from database
  entry <- rows(db, db$Package == package)
  if (nrow(entry) == 0L) {
    envir[[package]] <- NA_character_
    return()
  }

  # set discovered remote
  envir[[package]] <- with(entry, paste(Package, Version, sep = "@"))

  # iterate through hard dependencies
  fields <- c("Depends", "Imports", "LinkingTo")
  for (field in fields) {
    value <- entry[[field]]
    if (!is.null(value) && !is.na(value)) {
      value <- renv_description_parse_field(entry[[field]])
      for (package in value$Package)
        if (is.null(envir[[package]]))
          renv_checkout_recdeps_impl(package, db, envir)
    }
  }

  # for soft dependencies, only include those if they're currently installed
  # TODO: or check if it's in the lockfile?
  value <- entry[["Suggests"]]
  if (!is.null(value) && !is.na(value)) {
    value <- renv_description_parse_field(value)
    for (package in value$Package)
      if (is.null(envir[[package]]))
        if (renv_package_installed(package))
          renv_checkout_recdeps_impl(package, db, envir)
  }

}

renv_checkout_repos <- function(date) {

  # if no date was provided, just use default repositories
  if (is.null(date))
    return(getOption("repos"))

  # build path to repository snapshot location
  root <- dirname(config$ppm.url())
  url <- file.path(root, date)
  if (renv_download_available(file.path(url, "src/contrib/PACKAGES")))
    return(c(PPM = url))

  # requested date not available; try to search a bit
  candidate <- date
  for (i in 1:7) {
    candidate <- format(as.Date(candidate) - 1L)
    url <- file.path(root, candidate)
    if (renv_download_available(file.path(url, "src/contrib/PACKAGES"))) {
      fmt <- "- Snapshot date '%s' not available; using '%s' instead"
      printf(fmt, date, candidate)
      return(c(PPM = url))
    }
  }

  stopf("repository snapshot '%s' not available", date)

}
