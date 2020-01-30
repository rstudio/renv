
renv_update_find_repos <- function(records) {
  lapply(records, renv_update_find_repos_impl)
}

renv_update_find_repos_impl <- function(record) {

  latest <- renv_available_packages_latest(record$Package)
  if (version_compare(latest$Version, record$Version) == 1)
    return(latest)

  NULL

}

renv_update_find_github <- function(records) {

  # check for GITHUB_PAT
  if (is.na(Sys.getenv("GITHUB_PAT", unset = NA))) {

    msg <- paste(
      "GITHUB_PAT is unset. Updates may fail due to GitHub's API rate limit.",
      "",
      "To increase your GitHub API rate limit:",
      "- Use `usethis::browse_github_pat()` to create a Personal Access Token (PAT).",
      "- Use `usethis::edit_r_environ()` and add the token as `GITHUB_PAT`.",
      sep = "\n"
    )

    warning(msg, call. = FALSE)

  }

  renv_parallel_exec(records, renv_update_find_github_impl)

}

renv_update_find_github_impl <- function(record) {

  # validate we have a ref
  if (!renv_record_validate(record))
    return(NULL)

  # construct and parse record entry
  host   <- record$RemoteHost
  user   <- record$RemoteUsername
  repo   <- record$RemoteRepo
  subdir <- record$RemoteSubdir
  ref    <- record$RemoteRef

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

renv_update_find <- function(records) {

  sources <- extract_chr(records, "Source")
  grouped <- split(records, sources)

  # retrieve updates
  results <- enumerate(grouped, function(source, records) {
    case(
      source == "Repository" ~ renv_update_find_repos(records),
      source == "GitHub"     ~ renv_update_find_github(records)
    )
  })

  # remove groupings
  ungrouped <- unlist(results, recursive = FALSE, use.names = FALSE)

  updates <- Filter(Negate(is.null), ungrouped)
  names(updates) <- extract_chr(updates, "Package")
  renv_records_sort(updates)

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
#' @param check Boolean; check for package updates without actually
#'   installing available updates?
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
                   check   = FALSE,
                   confirm = interactive(),
                   project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_disallow(...)

  # get package records
  library <- library %||% renv_libpaths_all()
  records <- renv_snapshot_r_packages(library = library, project = project)
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
    named(lapply(missing, renv_available_packages_latest), missing)
  )

  vprintf("* Checking for updated packages ... ")

  # remove records that appear to be from an R package repository,
  # but are not actually available in the current repositories
  selected <- Filter(function(record) {

    source <- renv_record_source(record)
    if (!source %in% c("cran", "repository"))
      return(TRUE)

    # check for package in one of the active binary / source repos
    for (type in renv_package_pkgtypes()) {

      package <- record$Package
      entry <- catch(
        renv_available_packages_entry(
          package = package,
          type = type,
          quiet = TRUE
        )
      )

      if (!inherits(entry, "error"))
        return(TRUE)

    }

    # not found; return FALSE
    FALSE

  }, selected)

  for (type in renv_package_pkgtypes())
    renv_available_packages(type = type, quiet = TRUE)

  updates <- renv_update_find(selected)
  vwritef("Done!")

  if (empty(updates)) {
    vwritef("* All packages are up-to-date.")
    return(invisible(TRUE))
  }

  # perform a diff (for reporting to user)
  old <- selected[names(updates)]
  new <- updates
  diff <- renv_lockfile_diff_packages(old, new)

  # if we're only checking for updates, just report and exit
  if (check) {

    fmt <- case(
      length(diff) == 1 ~ "* %i package has updates available.",
      length(diff) != 1 ~ "* %i packages have updates available."
    )

    vwritef(fmt, length(diff))
    updates <- renv_updates(diff = diff, old = old, new = new)
    return(updates)

  }

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
