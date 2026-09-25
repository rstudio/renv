
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

# check each record for updates, in parallel if possible; errors are collected,
# to be reported under the given key
renv_update_find_parallel <- function(records, key, callback) {

  names(records) <- map_chr(records, `[[`, "Package")
  results <- renv_parallel_exec(records, function(record) {
    catch(callback(record))
  })

  failed <- map_lgl(results, inherits, "error")
  if (any(failed))
    renv_update_errors_set(key, results[failed])

  results[!failed]

}

renv_update_find_git <- function(records) {

  renv_scope_git_clones()
  results <- renv_update_find_parallel(records, "git", renv_update_find_git_impl)

  # the checks may have run in forked processes, whose clones this process
  # doesn't know about; adopt them, so that installing an update can re-use
  # its clone, and so that the clones are removed along with the others
  map(results, function(result) {

    if (is.null(result))
      return(NULL)

    renv_git_clone_adopt(result$record, result$path)
    if (result$updated)
      result$record

  })

}

renv_update_find_git_impl <- function(record) {

  # no sha is found if the ref couldn't be found on the remote; that's expected
  # when the ref is itself a commit, which has no updates, but otherwise means
  # the ref no longer exists (e.g. a deleted branch)
  shas <- renv_remotes_resolve_git_sha_ref(record)
  if (empty(shas)) {

    ref <- record$RemoteRef %||% ""
    if (grepl("^[[:xdigit:]]{7,64}$", ref))
      return(NULL)

    fmt <- "ref '%s' was not found in git repository '%s'"
    stopf(fmt, ref, record$RemoteUrl)

  }

  # for an annotated tag, remotes records the sha of the tag itself, rather
  # than that of the commit it points at, so accept either
  sha <- record$RemoteSha %||% ""
  if (sha %in% shas)
    return(NULL)

  current <- record
  current$RemoteSha <- shas[[1L]]

  path <- renv_remotes_resolve_git_clone(current)
  desc <- renv_description_read(path, subdir = current$RemoteSubdir)

  current$Version <- desc$Version
  current$Package <- desc$Package

  # without a recorded commit, we can't tell whether the installed package is
  # out of date, so only report an update if a newer version is available
  compare <- renv_version_compare(current$Version, record$Version)
  updated <- if (nzchar(sha)) compare >= 0L else compare > 0L

  list(record = current, path = path, updated = updated)

}

renv_update_find_github <- function(records) {
  renv_update_find_parallel(records, "github", renv_update_find_github_impl)
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

  url <- record$RemoteUrl %||% {
    origin <- fsub("api.github.com", "github.com", renv_retrieve_origin(host))
    parts <- c(origin, user, repo)
    paste(parts, collapse = "/")
  }

  # get updated record
  desc <- renv_remotes_resolve_github_description(url, host, user, repo, subdir, sha)
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

  renv_update_find_parallel(records, type, function(record) {
    renv_update_find_remote_impl(record, update)
  })

}

renv_update_find_remote_impl <- function(record, update) {

  remote <- list(
    host = record$RemoteHost,
    user = record$RemoteUsername,
    repo = record$RemoteRepo,
    subdir = record$RemoteSubdir,
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

  # group on the normalized source, since the same source can be spelled in
  # different ways; e.g. packages installed by renv from git have Source 'git',
  # whereas those installed by remotes (via 'git2r' or 'xgit') have Source 'Git'
  sources <- map_chr(records, renv_record_source, normalize = TRUE)
  grouped <- split(records, sources)

  # retrieve updates
  results <- enumerate(grouped, function(source, records) {
    case(
      source == "bioconductor" ~ renv_update_find_repos(records),
      source == "repository"   ~ renv_update_find_repos(records),
      source == "github"       ~ renv_update_find_github(records),
      source == "git"          ~ renv_update_find_git(records),
      source == "gitlab"       ~ renv_update_find_remote(records, "gitlab"),
      source == "bitbucket"    ~ renv_update_find_remote(records, "bitbucket")
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
#' @param lock Boolean; update the `renv.lock` lockfile after the successful
#'   installation of the requested packages?
#'
#' @param all Boolean; should `renv` check all library paths for out-of-date
#'   packages? When `FALSE` (the default), only the project library will be
#'   checked for out-of-date packages.
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
                   type    = NULL,
                   rebuild = FALSE,
                   check   = FALSE,
                   prompt  = interactive(),
                   lock    = FALSE,
                   all     = FALSE,
                   project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  # resolve library path
  library <- renv_libpaths_resolve(library)
  renv_scope_libpaths(library)

  # check for explicitly-provided type -- we handle this specially for PPM
  if (!is.null(type)) {
    renv_scope_binding(the, "install_pkg_type", type)
    renv_scope_options(pkgType = type)
  }

  # resolve exclusions
  exclude <- c(exclude, settings$ignored.packages(project = project))

  # if users have requested the use of pak, delegate there
  if (config$pak.enabled() && !recursing()) {
    packages <- setdiff(packages, exclude)
    renv_pak_init()
    return(
      renv_pak_install(
        packages = packages,
        library  = library,
        rebuild  = rebuild,
        type     = type,
        prompt   = prompt,
        project  = project
      )
    )
  }

  # get package records
  renv_scope_binding(the, "snapshot_hash", FALSE)
  libpaths <- if (all) library else library[[1L]]
  records <- renv_snapshot_libpaths(libpaths = libpaths, project = project)
  packages <- packages %||% names(records)

  # apply exclusions
  packages <- setdiff(packages, exclude)

  # check if the user has requested update for packages not installed
  missing <- renv_vector_diff(packages, names(records))
  if (!empty(missing)) {

    if (prompt || renv_verbose()) {
      bulletin(
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

  # share clones of git remotes between the update checks and the install
  renv_scope_git_clones()

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
    library  = library,
    rebuild  = rebuild,
    prompt   = FALSE,
    lock     = lock,
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
  renv_update_errors_emit_remote("git", "Git")
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

  bulletin(
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

