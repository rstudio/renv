
the$update_errors <- new.env(parent = emptyenv())

renv_update_find_repos <- function(records) {

  results <- lapply(records, function(record) {
    catch(renv_update_find_repos_impl(record))
  })

  failed <- map_lgl(results, inherits, "error")
  if (any(failed))
    renv_update_errors_set("repos", results[failed])

  results[!failed]

}

renv_update_find_repos_impl <- function(record) {

  # retrieve latest-available package
  package <- record$Package
  latest <- catch(renv_available_packages_latest(package))
  if (inherits(latest, "error"))
    return(NULL)

  # validate our versions
  if (empty(latest$Version) || empty(record$Version))
    return(NULL)

  # compare the versions; return NULL if the 'latest' version
  # is older
  compare <- renv_version_compare(latest$Version, record$Version)
  if (compare != 1L)
    return(NULL)

  latest

}

renv_update_find_git <- function(records) {
  renv_parallel_exec(records, renv_update_find_git_impl)
}

renv_update_find_git_impl <- function(record) {

  sha <- renv_remotes_resolve_git_sha_ref(record)

  # if sha is empty:
  # `git remote-ls origin ref` expects ref to be a reference, not a sha
  # it is empty if ref isn't a reference on the repo
  # this may be due to record$RemoteRef actually being a sha
  # or it may be because record$RemoteRef is not a real ref
  # but we can't check, so we will try to fetch the ref & see what we get
  oldsha <- record$RemoteSha %||% ""
  if (nzchar(oldsha) && identical(sha, oldsha))
    return(NULL)

  current <- record
  current$RemoteSha <- sha

  desc <- renv_remotes_resolve_git_description(current)

  current$Version <- desc$Version
  current$Package <- desc$Package

  updated <- renv_version_ge(current$Version, record$Version)
  if (updated)
    return(current)

}

renv_update_find_github <- function(records) {

  # check for GITHUB_PAT
  if (!renv_envvar_exists("GITHUB_PAT")) {

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

  names(records) <- map_chr(records, `[[`, "Package")
  results <- renv_parallel_exec(records, function(record) {
    catch(renv_update_find_github_impl(record))
  })

  failed <- map_lgl(results, inherits, "error")
  if (any(failed))
    renv_update_errors_set("github", results[failed])

  results[!failed]

}

renv_update_find_github_impl <- function(record) {

  # construct and parse record entry
  host   <- record$RemoteHost %||% config$github.host()
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


renv_update_find_remote <- function(records, type) {

  update <- switch(type,
    "gitlab" = renv_remotes_resolve_gitlab,
    "bitbucket" = renv_remotes_resolve_bitbucket,
    stopf("Unsupported type %s", type)
  )

  names(records) <- map_chr(records, `[[`, "Package")
  results <- renv_parallel_exec(records, function(record) {
    catch(renv_update_find_remote_impl(record, update))
  })

  failed <- map_lgl(results, inherits, "error")
  if (any(failed))
    renv_update_errors_set(type, results[failed])

  results[!failed]

}

renv_update_find_remote_impl <- function(record, update) {

  remote <- list(
    host = record$RemoteHost,
    user = record$RemoteUsername,
    repo = record$RemoteRepo,
    ref = record$RemoteRef
  )
  current <- update(remote)

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
      source == "Bioconductor" ~ renv_update_find_repos(records),
      source == "Repository"   ~ renv_update_find_repos(records),
      source == "GitHub"       ~ renv_update_find_github(records),
      source == "Git"          ~ renv_update_find_git(records),
      source == "GitLab"       ~ renv_update_find_remote(records, "gitlab"),
      source == "Bitbucket"    ~ renv_update_find_remote(records, "bitbucket")
    )
  })

  # remove groupings
  ungrouped <- unlist(results, recursive = FALSE, use.names = FALSE)
  if (empty(ungrouped))
    return(list())

  # keep non-null results
  updates <- Filter(Negate(is.null), ungrouped)
  if (empty(updates))
    return(list())

  names(updates) <- extract_chr(updates, "Package")
  renv_records_sort(updates)

}



#' Update packages
#'
#' @description
#' Update packages which are currently out-of-date. Currently supports CRAN,
#' Bioconductor, other CRAN-like repositories, GitHub, GitLab, Git, and
#' BitBucket.
#'
#' Updates will only be checked from the same source -- for example,
#' if a package was installed from GitHub, but a newer version is
#' available on CRAN, that updated version will not be seen.
#'
#' @inherit renv-params
#'
#' @param packages A character vector of \R packages to update. When `NULL`
#'   (the default), all packages (apart from any listed in the `ignored.packages`
#'   project setting) will be updated.
#'
#' @param check Boolean; check for package updates without actually
#'   installing available updates? This is useful when you'd like to determine
#'   what updates are available, without actually installing those updates.
#'
#' @param exclude A set of packages to explicitly exclude from updating.
#'   Use `renv::update(exclude = <...>)` to update all packages except for
#'   a specific set of excluded packages.
#'
#' @return A named list of package records which were installed by renv.
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
                   exclude = NULL,
                   library = NULL,
                   rebuild = FALSE,
                   check   = FALSE,
                   prompt  = interactive(),
                   project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  # resolve library path
  libpaths <- renv_libpaths_resolve(library)
  library <- nth(libpaths, 1L)
  renv_scope_libpaths(libpaths)

  # resolve exclusions
  exclude <- c(exclude, settings$ignored.packages(project = project))

  # if users have requested the use of pak, delegate there
  if (config$pak.enabled() && !recursing()) {
    packages <- setdiff(packages, exclude)
    renv_pak_init()
    return(renv_pak_install(packages, libpaths, project))
  }

  # get package records
  renv_scope_binding(the, "snapshot_hash", FALSE)
  records <- renv_snapshot_libpaths(libpaths = libpaths, project = project)
  packages <- packages %||% names(records)

  # apply exclusions
  packages <- setdiff(packages, exclude)

  # check if the user has requested update for packages not installed
  missing <- renv_vector_diff(packages, names(records))
  if (!empty(missing)) {

    if (prompt || renv_verbose()) {
      renv_pretty_print(
        "The following package(s) are not currently installed:",
        missing,
        "The latest available versions of these packages will be installed instead."
      )
    }

    cancel_if(prompt && !proceed())

  }

  # select records
  selected <- c(
    records[renv_vector_intersect(packages, names(records))],
    named(lapply(missing, renv_available_packages_latest), missing)
  )

  # check for usage of cran, bioc
  repo <- FALSE
  bioc <- FALSE

  for (record in selected) {

    source <- renv_record_source(record, normalize = TRUE)

    if (source %in% c("repository")) {
      repo <- TRUE
      next
    }

    if (source %in% c("bioconductor")) {
      repo <- bioc <- TRUE
      next
    }

  }

  # activate bioc repositories if needed
  if (bioc)
    renv_scope_bioconductor(project = project)

  # ensure database of available packages is current
  if (repo) {
    for (type in renv_package_pkgtypes()) {
      available_packages(type = type)
    }
  }

  printf("- Checking for updated packages ... ")

  # remove records that appear to be from an R package repository,
  # but are not actually available in the current repositories
  selected <- filter(selected, function(record) {

    source <- renv_record_source(record, normalize = TRUE)
    if (!source %in% c("bioconductor", "cran", "repository"))
      return(TRUE)

    # check for available package
    package <- record$Package
    entry <- catch(renv_available_packages_latest(package))
    !inherits(entry, "error")

  })

  updates <- renv_update_find(selected)
  writef("Done!")

  renv_update_errors_emit()

  if (empty(updates)) {
    writef("- All packages appear to be up-to-date.")
    return(invisible(TRUE))
  }

  # perform a diff (for reporting to user)
  old <- selected[names(updates)]
  new <- updates
  diff <- renv_lockfile_diff_packages(old, new)

  # if we're only checking for updates, just report and exit
  if (check) {

    fmt <- case(
      length(diff) == 1 ~ "- %i package has updates available.",
      length(diff) != 1 ~ "- %i packages have updates available."
    )

    preamble <- sprintf(fmt, length(diff))
    renv_updates_report(preamble, diff, old, new)
    return(invisible(renv_updates_create(diff, old, new)))

  }

  if (prompt || renv_verbose()) {
    renv_restore_report_actions(diff, old, new)
    cancel_if(prompt && !proceed())
  }

  # perform the install
  install(
    packages = updates,
    library  = libpaths,
    rebuild  = rebuild,
    project  = project
  )

}

renv_update_errors_set <- function(key, errors) {
  assign(key, errors, envir = the$update_errors)
}

renv_update_errors_clear <- function() {
  rm(
    list = ls(envir = the$update_errors, all.names = TRUE),
    envir = the$update_errors
  )
}

renv_update_errors_emit <- function() {

  # clear errors when we're done
  defer(renv_update_errors_clear())

  # if we have any errors, start by emitting a single newline
  all <- ls(envir = the$update_errors, all.names = TRUE)
  if (!empty(all))
    writef()

  # then emit errors for each class
  renv_update_errors_emit_repos()
  renv_update_errors_emit_remote("github", "GitHub")
  renv_update_errors_emit_remote("gitlab", "GitLab")
  renv_update_errors_emit_remote("bitbucket", "BitBucket")

}

renv_update_errors_emit_impl <- function(key, preamble, postamble) {

  errors <- the$update_errors[[key]]
  if (empty(errors))
    return()

  messages <- enumerate(errors, function(package, error) {
    errmsg <- paste(conditionMessage(error), collapse = "; ")
    sprintf("%s: %s", format(package), errmsg)
  })

  renv_pretty_print(
    preamble = preamble,
    values = messages,
    postamble = postamble
  )

}

renv_update_errors_emit_repos <- function() {

  renv_update_errors_emit_impl(
    key       = "repos",
    preamble  = "One or more errors occurred while finding updates for the following packages:",
    postamble = "Ensure that these packages are available from your active package repositories."
  )

}

renv_update_errors_emit_remote <- function(key, label) {

  renv_update_errors_emit_impl(
    key       = key,
    preamble  = sprintf("One or more errors occurred while finding updates for the following %s packages:", label),
    postamble = sprintf("Ensure that these packages were installed from an accessible %s remote.", label)
  )

}

