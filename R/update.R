
renv_update_find_repos <- function(record) {

  latest <- renv_records_repos_latest(record$Package)
  if (version_compare(latest$Version, record$Version) == 1)
    return(latest)

  NULL

}

renv_update_find_github <- function(record) {

  # validate we have a ref
  if (!renv_record_validate(record))
    return(NULL)

  # construct and parse record entry
  host   <- record$RemoteHost
  user   <- record$RemoteUsername
  repo   <- record$RemoteRepo
  subdir <- record$RemoteSubdir %||% ""
  ref    <- record$RemoteRef %||% "master"

  # check for changed sha
  sha <- renv_remotes_resolve_github_sha_ref(host, user, repo, ref)
  if (sha == record$RemoteSha)
    return(NULL)

  # get updated record
  desc <- renv_remotes_resolve_github_description(host, user, repo, subdir, sha)
  current <- list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "GitHub",
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha,
    RemoteHost     = host
  )

  # check that the version has actually updated
  updated <-
    current$RemoteSha != record$RemoteSha &&
    numeric_version(current$Version) >= numeric_version(record$Version)

  if (updated)
    return(current)

}

renv_update_find_impl <- function(record) {

  source <- tolower(record$Source %||% "unknown")

  case(
    source == "cran"       ~ renv_update_find_repos(record),
    source == "repository" ~ renv_update_find_repos(record),
    source == "github"     ~ renv_update_find_github(record)
  )

}

renv_update_find <- function(record) {

  status <- catch(renv_update_find_impl(record))
  if (inherits(status, "error")) {
    warning(status)
    return(NULL)
  }

  status

}

#' Update Packages
#'
#' Update packages which are currently out-of-date. Currently, only
#' CRAN and GitHub package sources are supported.
#'
#' Updates will only be checked from the same source -- for example,
#' if a package was installed from GitHub, but a newer version is
#' available on CRAN, that updated version will not be seen.
#'
#' @inherit renv-params
#' @inheritParams install-params
#'
#' @param packages A character vector of \R packages to update. When `NULL`,
#'   all packages within the required libraries will be updated.
#'
#' @return A named list of package records which were installed by `renv`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # update the 'dplyr' package
#' renv::update("dplyr")
#'
#' }
update <- function(packages = NULL,
                   ...,
                   library = NULL,
                   rebuild = FALSE,
                   confirm = interactive(),
                   project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()

  # get package records
  library <- library %||% renv_libpaths_default()
  records <- renv_snapshot_r_packages(library = library)
  packages <- packages %||% names(records)

  # check if the user has requested update for packages not installed
  missing <- renv_vector_diff(packages, names(records))
  if (!empty(missing)) {

    if (confirm || renv_verbose()) {
      renv_pretty_print(
        missing,
        "The following package(s) are not currently installed:",
        "The latest available versions of these packages will be installed instead.",
        wrap = FALSE
      )
    }

    if (confirm && !proceed()) {
      message("* Operation aborted.")
      return(invisible(FALSE))
    }

  }

  # select records
  selected <- c(
    records[renv_vector_intersect(packages, names(records))],
    named(lapply(missing, renv_records_repos_latest), missing)
  )

  # make sure we've requested available packages
  for (type in renv_package_pkgtypes())
    renv_available_packages(type = type)

  # construct new records to use for update
  vprintf("* Checking for updated packages ... ")
  find <- renv_progress(renv_update_find, length(records))
  updates <- Filter(is.list, lapply(selected, find))
  vwritef("Done!")

  if (empty(updates)) {
    vwritef("* All packages are up-to-date.")
    return(invisible(TRUE))
  }

  # perform a diff (for reporting to user)
  old <- selected[names(updates)]
  new <- updates
  diff <- renv_lockfile_diff_packages(old, new)

  if (confirm || renv_verbose())
    renv_restore_report_actions(diff, old, new)

  if (confirm && !proceed()) {
    message("* Operation aborted.")
    return(invisible(FALSE))
  }

  # perform the install
  install(
    updates,
    library = library,
    rebuild = rebuild,
    project = project
  )

}
