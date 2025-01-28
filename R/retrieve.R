
#' Retrieve packages
#'
#' Retrieve (download) one or more packages from external sources.
#' Using `renv::retrieve()` can be useful in CI / CD workflows, where
#' you might want to download all packages listed in a lockfile
#' before later invoking [renv::restore()]. Packages will be downloaded
#' to an internal path within `renv`'s local state directories -- see
#' [paths] for more details.
#'
#' If `destdir` is `NULL` and the requested package is already available
#' within the `renv` cache, `renv` will return the path to that package
#' directory in the cache.
#'
#' @inheritParams renv-params
#'
#' @param lockfile The path to an `renv` lockfile. When set, `renv`
#'   will retrieve the packages as defined within that lockfile.
#'   If `packages` is also non-`NULL`, then only those packages will
#'   be retrieved.
#'
#' @param destdir The directory where packages should be downloaded.
#'  When `NULL` (the default), the default internal storage locations
#'  (normally used by e.g. [renv::install()] or [renv::restore()]) will
#'  be used.
#'
#' @returns A named vector, mapping package names to the paths where
#'   those packages were downloaded.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # retrieve package + versions as defined in the lockfile
#' # normally used as a pre-flight step to renv::restore()
#' renv::retrieve()
#'
#' # download one or more packages locally
#' renv::retrieve("rlang", destdir = ".")
#'
#' }
retrieve <- function(packages = NULL,
                     ...,
                     lockfile = NULL,
                     destdir  = NULL,
                     project  = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  # set destdir if available
  if (!is.null(destdir)) {
    renv_scope_options(renv.config.cache.enabled = FALSE)
    renv_scope_binding(the, "destdir", destdir)
  }

  # figure out which records we want to retrieve
  if (is.null(packages) && is.null(lockfile)) {
    lockfile <- renv_lockfile_load(project = project)
    records <- renv_lockfile_records(lockfile)
    packages <- names(records)
  } else if (is.null(lockfile)) {
    records <- map(packages, renv_remotes_resolve, latest = TRUE)
    packages <- map_chr(records, `[[`, "Package")
    names(records) <- packages
  } else if (is.character(lockfile)) {
    lockfile <- renv_lockfile_read(lockfile)
    records <- renv_lockfile_records(lockfile)
    packages <- packages %||% names(records)
  }

  # overlay project remotes
  records <- overlay(renv_project_remotes(project), records)

  # perform the retrieval
  renv_scope_restore(
    project   = project,
    library   = library,
    packages  = packages,
    records   = records
  )

  result <- renv_retrieve_impl(packages)
  map_chr(result, `[[`, "Path")
}

renv_retrieve_impl <- function(packages) {

  # confirm that we have restore state set up
  state <- renv_restore_state()
  if (is.null(state))
    stopf("renv_restore_begin() must be called first")

  # normalize repositories (ensure @CRAN@ is resolved)
  options(repos = renv_repos_normalize())

  # transform repository URLs for PPM
  if (renv_ppm_enabled()) {
    repos <- getOption("repos")
    renv_scope_options(repos = renv_ppm_transform(repos))
  }

  # ensure HTTPUserAgent is set (required for PPM binaries)
  agent <- renv_http_useragent()
  if (!grepl("renv", agent)) {
    renv <- sprintf("renv (%s)", renv_metadata_version())
    agent <- paste(renv, agent, sep = "; ")
  }
  renv_scope_options(HTTPUserAgent = agent)

  before <- Sys.time()
  handler <- state$handler
  for (package in packages)
    handler(package, renv_retrieve_impl_one(package))
  after <- Sys.time()

  state <- renv_restore_state()
  count <- state$downloaded
  if (count) {
    elapsed <- difftime(after, before, units = "secs")
    writef("Successfully downloaded %s in %s.", nplural("package", count), renv_difftime_format(elapsed))
    writef("")
  }

  state$install$data()

}

renv_retrieve_impl_one <- function(package) {

  # skip packages with 'base' priority
  if (package %in% renv_packages_base())
    return()

  # if we've already attempted retrieval of this package, skip
  state <- renv_restore_state()
  if (!is.null(state$retrieved[[package]]))
    return()

  # insert a dummy value just to avoid infinite recursions
  # (this will get updated on a successful installation later)
  state$retrieved[[package]] <- NA

  # extract record for package
  records <- state$records
  record <- records[[package]] %||% renv_retrieve_resolve(package)

  # resolve lazy records
  if (is.function(record)) {
    state$records[[package]] <- resolve(record)
    record <- state$records[[package]]
  }

  # normalize the record source
  source <- renv_record_source(record, normalize = TRUE)

  # don't install packages from incompatible OS
  ostype <- tolower(record[["OS_type"]] %||% "")

  skip <-
    renv_platform_unix() && identical(ostype, "windows") ||
    renv_platform_windows() && identical(ostype, "unix")

  if (skip)
    return()

  # if this is a package from Bioconductor, activate those repositories now
  if (source %in% c("bioconductor")) {
    project <- renv_restore_state(key = "project")
    renv_scope_bioconductor(project = project)
  }

  # if this is a package from R-Forge, activate its repository
  if (source %in% c("repository")) {
    repository <- record$Repository %||% ""
    if (tolower(repository) %in% c("rforge", "r-forge")) {
      repos <- getOption("repos")
      if (!"R-Forge" %in% names(repos)) {
        repos[["R-Forge"]] <- "https://R-Forge.R-project.org"
        renv_scope_options(repos = repos)
      }
    }
  }

  # if the record doesn't declare the package version,
  # treat it as a request for the latest version on CRAN
  # TODO: should make this behavior configurable
  uselatest <-
    source %in% c("repository", "bioconductor") &&
    is.null(record$Version)

  if (uselatest) {
    record <- withCallingHandlers(
      renv_available_packages_latest(package),
      error = function(err) stopf("package '%s' is not available", package)
    )
  }

  # if the requested record is incompatible with the set
  # of requested package versions thus far, request the
  # latest version on the R package repositories
  #
  # TODO: handle more explicit dependency requirements
  # TODO: report to the user if they have explicitly requested
  # installation of this package version despite it being incompatible
  compat <- renv_retrieve_incompatible(package, record)
  if (NROW(compat)) {

    # get the latest available package version
    replacement <- renv_available_packages_latest(package)
    if (is.null(replacement))
      stopf("package '%s' is not available", package)

    # if it's not compatible, then we might need to try again with
    # a source version (assuming type = "both")
    pkgtype <- getOption("pkgType")
    if (identical(pkgtype, "both")) {
      iscompat <- renv_retrieve_incompatible(package, replacement)
      if (NROW(iscompat)) {
        replacement <- renv_available_packages_latest(package, type = "source")
      }
    }

    # report if we couldn't find a compatible package
    renv_retrieve_incompatible_report(package, record, replacement, compat)
    record <- replacement

  }

  rebuild <- renv_restore_rebuild_required(record)
  if (!rebuild) {

    # if we have an installed package matching the requested record, finish early
    path <- renv_restore_find(package, record)
    if (file.exists(path)) {
      install <- !dirname(path) %in% renv_libpaths_all()
      return(renv_retrieve_successful(record, path, install = install))
    }

    # if the requested record already exists in the cache,
    # we'll use that package for install
    cacheable <-
      renv_cache_config_enabled(project = state$project) &&
      renv_record_cacheable(record)

    if (cacheable) {

      # try to find the record in the cache
      path <- renv_cache_find(record)
      if (nzchar(path) && renv_cache_package_validate(path))
        return(renv_retrieve_successful(record, path))

    }

  }

  # if this is a URL source, then it should already have a local path
  # check for the Path and Source fields and see if they resolve
  fields <- c("Path", "Source")
  for (field in fields) {

    # check for a valid field
    path <- record[[field]]
    if (is.null(path))
      next

    # check whether it looks like an explicit source
    isurl <-
      is.character(path) &&
      nzchar(path) &&
      grepl("[/\\]|[.](?:zip|tgz|gz)$", path)

    if (!isurl)
      next

    # error if the field is declared but doesn't exist
    if (!file.exists(path)) {
      fmt <- "record for package '%s' declares local source '%s', but that file does not exist"
      stopf(fmt, record$Package, path)
    }

    # otherwise, success
    path <- renv_path_normalize(path, mustWork = TRUE)
    return(renv_retrieve_successful(record, path))

  }


  # try some early shortcut methods
  shortcuts <- if (rebuild) c(
    renv_retrieve_cellar
  ) else c(
    renv_retrieve_explicit,
    renv_retrieve_cellar,
    if (!renv_tests_running() && config$install.shortcuts())
      renv_retrieve_libpaths
  )

  for (shortcut in shortcuts) {
    retrieved <- catch(shortcut(record))
    if (identical(retrieved, TRUE))
      return(TRUE)
  }

  state$downloaded <- state$downloaded + 1L
  if (state$downloaded == 1L)
    writef(header("Downloading packages"))

  # time to retrieve -- delegate based on previously-determined source
  switch(source,
         bioconductor = renv_retrieve_bioconductor(record),
         bitbucket    = renv_retrieve_bitbucket(record),
         git          = renv_retrieve_git(record),
         github       = renv_retrieve_github(record),
         gitlab       = renv_retrieve_gitlab(record),
         repository   = renv_retrieve_repos(record),
         url          = renv_retrieve_url(record),
         renv_retrieve_unknown_source(record)
  )

}

renv_retrieve_name <- function(record, type = "source", ext = NULL) {
  package <- record$Package
  version <- record$RemoteSha %||% record$Version
  ext <- ext %||% renv_package_ext(type)
  sprintf("%s_%s%s", package, version, ext)
}

renv_retrieve_path <- function(record, type = "source", ext = NULL) {

  # extract relevant record information
  package <- record$Package
  name <- renv_retrieve_name(record, type, ext)

  # if we have a destdir override, use this
  if (!is.null(the$destdir))
    return(file.path(the$destdir, name))

  # check for packages from an PPM binary URL, and
  # update the package type if known
  source <- renv_record_source(record)
  if (renv_ppm_enabled()) {
    url <- attr(record, "url")
    if (is.character(url) && grepl("/__[^_]+__/", url))
      type <- "binary"
  }

  # form path for package to be downloaded
  if (type == "source")
    renv_paths_source(source, package, name)
  else if (type == "binary")
    renv_paths_binary(source, package, name)
  else
    stopf("unrecognized type '%s'", type)
}

renv_retrieve_bioconductor <- function(record) {

  # try to read the bioconductor version from the record
  version <- renv_retrieve_bioconductor_version(record)

  # activate Bioconductor repositories in this context
  project <- renv_restore_state(key = "project")
  renv_scope_bioconductor(project = project, version = version)

  # retrieve record using updated repositories
  renv_retrieve_repos(record)

}

renv_retrieve_bioconductor_version <- function(record) {

  # read git branch
  branch <- record[["git_branch"]]
  if (is.null(branch))
    return(NULL)

  # try and parse version
  parts <- strsplit(branch, "_", fixed = TRUE)[[1L]]
  ok <-
    length(parts) == 3L &&
    tolower(parts[[1L]]) == "release"

  if (!ok)
    return(NULL)

  # we have a version; use it
  paste(tail(parts, n = -1L), collapse = ".")

}

renv_retrieve_bitbucket <- function(record) {

  # query repositories endpoint to find download URL
  host <- record$RemoteHost %||% config$bitbucket.host()
  origin <- renv_retrieve_origin(host)
  username <- record$RemoteUsername
  repo <- record$RemoteRepo

  # scope authentication
  renv_scope_auth(repo)

  fmt <- "%s/repositories/%s/%s"
  url <- sprintf(fmt, origin, username, repo)

  destfile <- renv_scope_tempfile("renv-bitbucket-")
  download(url, destfile = destfile, quiet = TRUE)
  json <- renv_json_read(destfile)

  # now build URL to tarball
  base <- json$links$html$href
  ref <- record$RemoteSha %||% record$RemoteRef

  fmt <- "%s/get/%s.tar.gz"
  url <- sprintf(fmt, base, ref)

  path <- renv_retrieve_path(record)

  renv_retrieve_package(record, url, path)

}

renv_retrieve_github <- function(record) {

  host <- record$RemoteHost %||% config$github.host()
  origin <- renv_retrieve_origin(host)
  username <- record$RemoteUsername
  repo <- record$RemoteRepo
  ref <- record$RemoteSha %||% record$RemoteRef

  if (is.null(ref)) {
    fmt <- "GitHub record for package '%s' has no recorded 'RemoteSha' / 'RemoteRef'"
    stopf(fmt, record$Package)
  }

  fmt <- "%s/repos/%s/%s/tarball/%s"
  url <- with(record, sprintf(fmt, origin, username, repo, ref))
  path <- renv_retrieve_path(record)
  renv_retrieve_package(record, url, path)

}

renv_retrieve_gitlab <- function(record) {

  host <- record$RemoteHost %||% config$gitlab.host()
  origin <- renv_retrieve_origin(host)

  user <- record$RemoteUsername
  repo <- record$RemoteRepo
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)

  fmt <- "%s/api/v4/projects/%s/repository/archive.tar.gz"
  url <- sprintf(fmt, origin, id)
  path <- renv_retrieve_path(record)

  sha <- record$RemoteSha %||% record$RemoteRef
  if (!is.null(sha))
    url <- paste(url, paste("sha", sha, sep = "="), sep = "?")

  renv_retrieve_package(record, url, path)

}

renv_retrieve_git <- function(record) {
  # NOTE: This path will later be used during the install step, so we don't
  # want to clean it up afterwards
  path <- tempfile("renv-git-")
  ensure_directory(path)
  renv_retrieve_git_impl(record, path)
  renv_retrieve_successful(record, path)
}

renv_retrieve_git_impl <- function(record, path) {

  renv_git_preflight()

  package <- record$Package
  url     <- record$RemoteUrl
  ref     <- record$RemoteRef
  sha     <- record$RemoteSha

  # figure out the default ref
  gitref <- case(
    nzchar(sha %||% "") ~ sha,
    nzchar(ref %||% "") ~ ref,
    "HEAD"
  )

  # be quiet if requested
  quiet <- getOption("renv.git.quiet", default = TRUE)
  quiet <- if (quiet) "--quiet" else ""

  template <- heredoc('
    git init ${QUIET}
    git remote add origin "${ORIGIN}"
    git fetch ${QUIET} --depth=1 origin "${REF}"
    git reset ${QUIET} --hard FETCH_HEAD
  ')

  data <- list(
    ORIGIN = url,
    REF    = gitref,
    QUIET  = quiet
  )

  commands <- renv_template_replace(template, data)
  command <- gsub("\n", " && ", commands, fixed = TRUE)
  if (renv_platform_windows())
    command <- paste(comspec(), "/C", command)

  printf("- Cloning '%s' ... ", url)

  before <- Sys.time()

  status <- local({
    ensure_directory(path)
    renv_scope_wd(path)
    renv_scope_auth(record)
    renv_scope_git_auth()
    system(command)
  })

  after <- Sys.time()

  if (status != 0L) {
    fmt <- "error cloning '%s' from '%s' [status code %i]"
    stopf(fmt, package, url, status)
  }

  fmt <- "OK [cloned repository in %s]"
  elapsed <- difftime(after, before, units = "auto")
  writef(fmt, renv_difftime_format(elapsed))

  TRUE

}


renv_retrieve_cellar_find <- function(record, project = NULL) {

  project <- renv_project_resolve(project)

  # packages installed with 'remotes::install_local()' will
  # have a RemoteUrl entry that we can use
  url <- record$RemoteUrl %||% ""
  if (file.exists(url)) {
    path <- renv_path_normalize(url, mustWork = TRUE)
    type <- if (fileext(path) %in% c(".tgz", ".zip")) "binary" else "source"
    return(named(path, type))
  }

  # otherwise, look in the cellar
  roots <- renv_cellar_roots(project)
  for (type in c("binary", "source")) {

    name <- renv_retrieve_name(record, type = type)
    for (root in roots) {

      package <- record$Package
      paths <- c(
        file.path(root, package, name),
        file.path(root, name)
      )

      for (path in paths)
        if (file.exists(path))
          return(named(path, type))

    }
  }

  fmt <- "%s [%s] is not available locally"
  stopf(fmt, record$Package, record$Version)

}

renv_retrieve_cellar_report <- function(record) {

  source <- renv_record_source(record)
  if (source == "cellar")
    return(record)

  fmt <- "- Package %s [%s] will be installed from the cellar."
  with(record, writef(fmt, Package, Version))

  record

}

renv_retrieve_cellar <- function(record) {
  source <- renv_retrieve_cellar_find(record)
  record <- renv_retrieve_cellar_report(record)
  renv_retrieve_successful(record, source)
}

renv_retrieve_libpaths <- function(record) {

  libpaths <- c(renv_libpaths_user(), renv_libpaths_site())
  for (libpath in libpaths)
    if (renv_retrieve_libpaths_impl(record, libpath))
      return(TRUE)

}

renv_retrieve_libpaths_impl <- function(record, libpath) {

  # form path to installed package's DESCRIPTION
  path <- file.path(libpath, record$Package)
  if (!file.exists(path))
    return(FALSE)

  # read DESCRIPTION
  desc <- renv_description_read(path = path)

  # check if it's compatible with the requested record
  fields <- c("Package", "Version", grep("^Remote", names(record), value = TRUE))
  compatible <- identical(record[fields], desc[fields])
  if (!compatible)
    return(FALSE)

  # check that it was built for a compatible version of R
  built <- desc[["Built"]]
  if (is.null(built))
    return(FALSE)

  ok <- catch(renv_description_built_version(desc))
  if (!identical(ok, TRUE))
    return(FALSE)

  # check that this package has a known source
  source <- renv_snapshot_description_source(desc)
  if (identical(source$Source, "unknown"))
    return(FALSE)

  # OK: copy this package as-is
  renv_retrieve_successful(record, path)

}

renv_retrieve_explicit <- function(record) {

  # try parsing as a local remote
  source <- record$Path %||% record$RemoteUrl %||% ""
  if (nzchar(source)) {
    resolved <- catch(renv_remotes_resolve_path(source))
    if (inherits(resolved, "error"))
      return(FALSE)
  }

  # treat as 'local' source but extract path
  normalized <- renv_path_normalize(source, mustWork = TRUE)
  resolved$Source <- "Local"
  renv_retrieve_successful(resolved, normalized)

}

renv_retrieve_repos <- function(record) {

  # if this record is tagged with a type + url, we can
  # use that directly for retrieval
  if (renv_record_tagged(record))
    return(renv_retrieve_repos_impl(record))

  # figure out what package sources are okay to use here
  pkgtype <- getOption("pkgType", default = "source")

  srcok <-
    pkgtype %in% c("both", "source") ||
    getOption("install.packages.check.source", default = "yes") %in% "yes"

  binok <-
    pkgtype %in% c("both", "binary") ||
    grepl("binary", pkgtype, fixed = TRUE)

  # collect list of 'methods' for retrieval
  methods <- stack(mode = "list")

  # add binary package methods
  if (binok) {

    # prefer repository binaries if available
    methods$push(renv_retrieve_repos_binary)

    # also try fallback binary locations (for Nexus)
    methods$push(renv_retrieve_repos_binary_fallback)

    # if p3m is enabled, check those binaries as well
    if (renv_p3m_enabled())
      methods$push(renv_retrieve_repos_p3m)

  }

  # next, try to retrieve from sources
  if (srcok) {

    # retrieve from source repositories
    methods$push(renv_retrieve_repos_source)

    # also try fallback source locations (for Nexus)
    methods$push(renv_retrieve_repos_source_fallback)

    # if this is a package from r-universe, try restoring from github
    # (currently inferred from presence for RemoteUrl field)
    unifields <- c("RemoteUrl", "RemoteSha")
    if (all(unifields %in% names(record)))
      methods$push(renv_retrieve_git)
    else
      methods$push(renv_retrieve_repos_archive)

  }

  # capture errors for reporting
  errors <- stack()

  for (method in methods$data()) {

    status <- catch(
      withCallingHandlers(
        method(record),
        renv.retrieve.error = function(error) {
          errors$push(error$data)
        }
      )
    )

    if (inherits(status, "error")) {
      errors$push(status)
      next
    }

    if (identical(status, TRUE))
      return(TRUE)

    if (!is.logical(status)) {
      fmt <- "internal error: unexpected status code '%s'"
      warningf(fmt, stringify(status))
    }

  }

  # if we couldn't download the package, report the errors we saw
  local({
    renv_scope_options(warn = 1L)
    for (error in errors$data())
      warning(error)
  })

  remote <- renv_record_format_remote(record, compact = TRUE)
  stopf("failed to retrieve package '%s'", remote)

}

renv_retrieve_repos_error_report <- function(record, errors) {

  if (empty(errors))
    return()

  messages <- extract(errors, "message")
  if (empty(messages))
    return()

  messages <- unlist(messages, recursive = TRUE, use.names = FALSE)
  if (empty(messages))
    return()

  fmt <- "The following error(s) occurred while retrieving '%s':"
  preamble <- sprintf(fmt, record$Package)

  caution_bullets(
    preamble = preamble,
    values   = paste("-", messages)
  )

  if (renv_verbose())
    str(errors)

}

renv_retrieve_url_resolve <- function(record) {

  # https://github.com/rstudio/renv/issues/2060
  pkgref <- record$RemotePkgRef
  if (!is.null(pkgref)) {
    remote <- renv_remotes_parse(pkgref)
    if (identical(remote$type, "url"))
      return(remote$url)
  }

  record$RemoteUrl

}

renv_retrieve_url <- function(record) {
  url <- renv_retrieve_url_resolve(record)
  resolved <- renv_remotes_resolve_url(url, quiet = FALSE)
  renv_retrieve_successful(record, resolved$Path)
}

renv_retrieve_repos_archive_name <- function(record, type = "source") {

  file <- record$File
  if (length(file) && !is.na(file))
    return(file)

  ext <- renv_package_ext(type)
  paste0(record$Package, "_", record$Version, ext)

}

renv_retrieve_repos_p3m <- function(record) {

  # TODO: support Linux
  if (renv_platform_linux())
    return(FALSE)

  # ensure local database is up-to-date
  renv_p3m_database_refresh(explicit = FALSE)

  # check that we have an available database
  path <- renv_p3m_database_path()
  if (!file.exists(path))
    return(FALSE)

  # attempt to read it
  database <- catch(renv_p3m_database_load())
  if (inherits(database, "error")) {
    warning(database)
    return(FALSE)
  }

  # get entry for this version of R + platform
  suffix <- contrib.url("", type = "binary")
  entry <- database[[suffix]]
  if (is.null(entry))
    return(FALSE)

  # check for known entry for this package + version
  key <- paste(record$Package, record$Version)
  idate <- entry[[key]]
  if (is.null(idate))
    return(FALSE)

  # convert from integer to date
  date <- as.Date(idate, origin = "1970-01-01")

  # form url to binary package
  base <- renv_p3m_url(date, suffix)
  name <- renv_retrieve_name(record, type = "binary")
  url <- file.path(base, name)

  # form path to saved file
  path <- renv_retrieve_path(record, "binary")

  # tag record with repository name
  record <- overlay(record, list(
    Source = "Repository",
    Repository = "P3M"
  ))

  # attempt to retrieve
  renv_retrieve_package(record, url, path)

}

renv_retrieve_repos_binary <- function(record) {
  renv_retrieve_repos_impl(record, "binary")
}

renv_retrieve_repos_binary_fallback <- function(record) {

  for (repo in getOption("repos")) {
    if (renv_nexus_enabled(repo)) {
      repourl <- contrib.url(repo, type = "binary")
      status <- catch(renv_retrieve_repos_impl(record, "binary", repo = repourl))
      if (!inherits(status, "error"))
        return(status)
    }
  }

  FALSE

}

renv_retrieve_repos_source <- function(record) {
  renv_retrieve_repos_impl(record, "source")
}

renv_retrieve_repos_source_fallback <- function(record, repo) {

  for (repo in getOption("repos")) {
    if (renv_nexus_enabled(repo)) {
      repourl <- contrib.url(repo, type = "source")
      status <- catch(renv_retrieve_repos_impl(record, "source", repo = repourl))
      if (!inherits(status, "error"))
        return(status)
    }
  }

  FALSE

}

renv_retrieve_repos_archive <- function(record) {

  # get the current repositories
  repos <- getOption("repos")

  # if this record has a repository recorded, use or prefer it
  repository <- record[["Repository"]]
  if (is.character(repository)) {
    names(repository) <- names(repository) %||% repository
    if (grepl("://", repository, fixed = TRUE)) {
      repos <- c(repository, repos)
    } else if (repository %in% names(repos)) {
      matches <- names(repos) == repository
      repos <- c(repos[matches], repos[!matches])
    }
  }

  for (repo in repos) {

    # try to determine path to package in archive
    root <- renv_retrieve_repos_archive_root(repo, record)
    if (is.null(root))
      next

    # attempt download; report errors via condition handler
    name <- renv_retrieve_repos_archive_name(record, type = "source")
    status <- catch(renv_retrieve_repos_impl(record, "source", name, root))
    if (inherits(status, "error")) {
      attr(status, "record") <- record
      renv_condition_signal("renv.retrieve.error", status)
    }

    # exit now if we had success
    if (identical(status, TRUE))
      return(TRUE)

  }

  return(FALSE)

}

renv_retrieve_repos_archive_root <- function(url, record) {

  # allow users to provide a custom archive path for a record,
  # in case they're using a repository that happens to archive
  # packages with a different format than regular CRAN network
  # https://github.com/rstudio/renv/issues/602
  override <- getOption("renv.retrieve.repos.archive.path")
  if (is.function(override)) {
    result <- override(url, record)
    if (!is.null(result))
      return(result)
  }

  # retrieve the appropriate formatter for this repository url
  formatter <- memoize(
    key   = url,
    value = renv_retrieve_repos_archive_formatter(url)
  )

  # use it
  formatter(url, record)

}

renv_retrieve_repos_archive_formatter <- function(url) {

  # list of known formatters
  formatters <- list(

    # default CRAN format
    cran = function(repo, record) {
      with(record, file.path(repo, "src/contrib/Archive", Package))
    },

    # format used by older releases of Artifactory
    # https://github.com/rstudio/renv/issues/602
    # https://github.com/rstudio/renv/issues/1996
    artifactory = function(repo, record) {
      with(record, file.path(repo, "src/contrib/Archive", Package, Version))
    },

    # format used by Nexus
    # https://github.com/rstudio/renv/issues/595
    nexus = function(repo, record) {
      with(record, file.path(repo, "src/contrib"))
    }

  )

  # check for an override
  override <- getOption("renv.repos.formatters")
  if (!is.null(override)) {
    formatter <- formatters[[override[[url]] %||% ""]]
    if (!is.null(formatter))
      return(formatter)
  }

  # build URL to PACKAGES file in src/contrib
  pkgurl <- file.path(url, "src/contrib/PACKAGES")
  headers <- renv_download_headers(pkgurl)

  # use the headers to infer the repository type
  if ("x-artifactory-id" %in% names(headers)) {
    formatters[["cran"]]
  } else if (grepl("Nexus", headers[["server"]] %||% "")) {
    formatters[["nexus"]]
  } else {
    formatters[["cran"]]
  }

}

# NOTE: If 'repo' is provided, it should be the path to the appropriate 'arm'
# of a repository, which is normally generated from the repository URL via
# 'contrib.url()'.
renv_retrieve_repos_impl <- function(record,
                                     type = NULL,
                                     name = NULL,
                                     repo = NULL)
{
  package <- record$Package
  version <- record$Version

  type <- type %||% attr(record, "type", exact = TRUE)
  name <- name %||% renv_retrieve_repos_archive_name(record, type)
  repo <- repo %||% attr(record, "url", exact = TRUE)

  # if we weren't provided a repository for this package, try to find it
  if (is.null(repo)) {

    entry <- catch(
      renv_available_packages_entry(
        package = package,
        type    = type,
        filter  = version,
        prefer  = record[["Repository"]]
      )
    )

    if (inherits(entry, "error")) {
      attr(entry, "record") <- record
      renv_condition_signal("renv.retrieve.error", entry)
      return(FALSE)
    }

    # get repository path
    repo <- entry$Repository

    # add in the path if available
    path <- entry$Path
    if (length(path) && !is.na(path))
      repo <- file.path(repo, path)

    # update the tarball name if it was declared
    file <- entry$File
    if (length(file) && !is.na(file))
      name <- file

  }

  url <- file.path(repo, name)
  path <- renv_retrieve_path(record, type)

  renv_retrieve_package(record, url, path)

}


renv_retrieve_package <- function(record, url, path) {

  ensure_parent_directory(path)
  type <- renv_record_source(record)
  status <- local({
    renv_scope_auth(record)
    preamble <- renv_retrieve_package_preamble(record, url)
    catch(download(url, preamble = preamble, destfile = path, type = type))
  })

  # report error for logging upstream
  if (inherits(status, "error")) {
    attr(status, "record") <- record
    renv_condition_signal("renv.retrieve.error", status)
  }

  # handle FALSE returns (shouldn't normally happen?)
  if (identical(status, FALSE)) {
    fmt <- "an unknown error occurred installing '%s' (%s)"
    msg <- sprintf(fmt, record$Package, renv_record_format_remote(record))
    status <- simpleError(msg)
  }

  # handle errors
  if (inherits(status, "error"))
    stop(status)

  # handle success
  renv_retrieve_successful(record, path)

}

renv_retrieve_package_preamble <- function(record, url) {

  message <- sprintf(
    "- Downloading %s from %s ... ",
    record$Package,
    record$Repository %||% record$Source
  )

  format(message, width = the$install_step_width)

}

renv_retrieve_successful_subdir <- function(record, path) {

  # if it's a file, assume RemoteSubdir needs to be honored
  info <- file.info(path, extra_cols = FALSE)
  if (identical(info$isdir, FALSE))
    return(record$RemoteSubdir)

  # otherwise, respect RemoteSubdir only if it seems to
  # point at a valid DESCRPITION file
  if (!is.null(record$RemoteSubdir)) {
    parts <- c(path, record$RemoteSubdir, "DESCRIPTION")
    descpath <- paste(parts, collapse = "/")
    if (file.exists(descpath))
      return(record$RemoteSubdir)
  }

}

renv_retrieve_successful <- function(record, path, install = TRUE) {

  # if we downloaded an archive, adjust its permissions here
  mode <- Sys.getenv("RENV_CACHE_MODE", unset = NA)
  if (!is.na(mode)) {
    info <- file.info(path, extra_cols = FALSE)
    if (identical(info$isdir, FALSE)) {
      parent <- dirname(path)
      renv_system_exec(
        command = "chmod",
        args    = c("-Rf", renv_shell_quote(mode), renv_shell_path(parent)),
        action  = "chmoding cached package",
        quiet   = TRUE,
        success = NULL
      )
    }
  }

  # the handling of 'subdir' here is a little awkward, as this function
  # can receive:
  #
  # - archives, whose package might live within a sub-directory;
  # - folders, whose package might live within a sub-directory;
  # - cache paths, for which the subdir is no longer relevant
  #
  # this warrants a proper cleanup, but for now we we use a hack
  subdir <- renv_retrieve_successful_subdir(record, path)

  # augment record with information from DESCRIPTION file
  desc <- renv_description_read(path, subdir = subdir)

  # update the record's package name, version
  # TODO: should we warn if they didn't match for some reason?
  package <- record$Package <- desc$Package
  record$Version <- desc$Version

  # add in path information to record (used later during install)
  record$Path <- path

  # add information on the retrieved record
  state <- renv_restore_state()
  state$retrieved[[package]] <- record

  # record this package's requirements
  requirements <- state$requirements

  # figure out the dependency fields to use -- if the user explicitly requested
  # this package be installed, but also provided a 'dependencies' argument in
  # the call to 'install()', then we want to use those
  fields <- if (package %in% state$packages) the$install_dependency_fields else "strong"
  deps <- renv_dependencies_discover_description(path, subdir = subdir, fields = fields)
  if (length(deps$Source))
    deps$Source <- record$Package

  rowapply(deps, function(dep) {
    package <- dep[["Package"]]
    requirements[[package]] <- requirements[[package]] %||% stack()
    requirements[[package]]$push(dep)
  })

  # read and handle remotes declared by this package
  remotes <- desc$Remotes
  if (length(remotes) && config$install.remotes())
    renv_retrieve_remotes(remotes)

  # ensure its dependencies are retrieved as well
  if (state$recursive) local({
    repos <- if (is.null(desc$biocViews)) getOption("repos") else renv_bioconductor_repos()
    renv_scope_options(repos = repos)
    renv_retrieve_successful_recurse(deps)
  })

  # mark package as requiring install if needed
  if (install && !state$install$contains(package))
    state$install$insert(package, record)

  TRUE

}

renv_retrieve_successful_recurse <- function(deps) {
  remotes <- setdiff(unique(deps$Package), renv_packages_base())
  for (remote in remotes)
    renv_retrieve_successful_recurse_impl(remote)
}

renv_retrieve_successful_recurse_impl_check <- function(remote) {

  # only done for package names
  if (!grepl(renv_regexps_package_name(), remote))
    return(FALSE)

  # check whether this package has been retrieved yet
  state <- renv_restore_state()
  record <- state$retrieved[[remote]]
  if (is.null(record) || identical(record, NA))
    return(FALSE)

  # check the current requirements for this package
  incompat <- renv_retrieve_incompatible(remote, record)
  if (NROW(incompat) == 0L)
    return(FALSE)

  # we have an incompatible record; ensure it gets retrieved
  state$retrieved[[remote]] <- NULL
  TRUE

}

renv_retrieve_successful_recurse_impl <- function(remote) {

  # if remote is a plain package name that we've already retrieved,
  # we may need to retrieve it again if the version of that package
  # required is greater than the previously-obtained version
  #
  # TODO: implement a proper solver so we can stop doing these hacks...
  # if this is a 'plain' package remote, retrieve it
  force <- renv_retrieve_successful_recurse_impl_check(remote)

  dynamic(
    key   = list(remote = remote),
    value = renv_retrieve_successful_recurse_impl_one(remote),
    force = force
  )

}

renv_retrieve_successful_recurse_impl_one <- function(remote) {

  # ignore base packages
  base <- renv_packages_base()
  if (remote %in% base)
    return(list())

  # if this is a 'plain' package remote, retrieve it
  if (grepl(renv_regexps_package_name(), remote)) {
    renv_retrieve_impl_one(remote)
    return(list())
  }

  # otherwise, handle custom remotes
  record <- renv_retrieve_remotes_impl(remote)
  if (length(record)) {
    renv_retrieve_impl_one(record$Package)
    return(list())
  }

  list()

}

renv_retrieve_unknown_source <- function(record) {

  # try to find a matching local package
  status <- catch(renv_retrieve_cellar(record))
  if (!inherits(status, "error"))
    return(status)

  # failed; parse as though from R package repository
  record$Source <- "Repository"
  renv_retrieve_repos(record)

}

# TODO: what should we do if we detect incompatible remotes?
# e.g. if pkg A requests 'r-lib/rlang@0.3' but pkg B requests
# 'r-lib/rlang@0.2'.
renv_retrieve_remotes <- function(remotes) {
  remotes <- strsplit(remotes, "\\s*,\\s*")[[1L]]
  for (remote in remotes)
    renv_retrieve_remotes_impl(remote)
}

renv_retrieve_remotes_impl <- function(remote) {

  dynamic(
    key   = list(remote = remote),
    value = renv_retrieve_remotes_impl_one(remote)
  )

}

renv_retrieve_remotes_impl_one <- function(remote) {

  # TODO: allow customization of behavior when remote parsing fails?
  resolved <- catch(renv_remotes_resolve(remote))
  if (inherits(resolved, "error")) {
    warningf("failed to resolve remote '%s'; skipping", remote)
    return(invisible(NULL))
  }

  # get the current package record
  state <- renv_restore_state()
  package <- resolved$Package
  record <- state$records[[package]]

  # if we already have a package record, and it's not a 'plain'
  # repository record, skip
  skip <-
    !is.null(record) &&
    !identical(record, list(Package = package, Source = "Repository"))

  if (skip) {
    dlog("retrieve", "skipping remote '%s'; it's already been declared", remote)
    dlog("retrieve", "using existing remote '%s'", stringify(record))
    return(invisible(NULL))
  }

  # update the requested record
  dlog("retrieve", "using remote '%s'", remote)
  state$records[[package]] <- resolved

  # mark the record as needing retrieval
  state$retrieved[[package]] <- NULL

  # return new record
  invisible(resolved)

}

renv_retrieve_resolve <- function(package) {
  tryCatch(
    renv_snapshot_description(package = package),
    error = function(e) {
      renv_retrieve_missing_record(package)
    }
  )
}

renv_retrieve_missing_record <- function(package) {

  # TODO: allow users to configure the action to take here, e.g.
  #
  #   1. retrieve latest from R repositories (the default),
  #   2. request a package + version to be retrieved,
  #   3. hard error
  #
  record <- renv_available_packages_latest(package)
  if (!is.null(record))
    return(record)

  fmt <- heredoc("
    renv was unable to find a compatible version of package '%1$s'.

    The latest-available version %1$s is '%2$s', but that version
    does not appear to be compatible with this version of R.

    You may need to manually re-install a different version of '%1$s'.
  ")

  entry <- renv_available_packages_entry(package, type = "source")
  version <- entry$Version %||% "<unknown>"

  writef(fmt, package, version)

  stopf("failed to find a compatible version of the '%s' package", package)

}

# check to see if this requested record is incompatible
# with the set of required dependencies recorded thus far
# during the package retrieval process
renv_retrieve_incompatible <- function(package, record) {

  state <- renv_restore_state()
  record <- renv_record_validate(package, record)

  # check and see if the installed version satisfies all requirements
  requirements <- state$requirements[[package]]
  if (is.null(requirements))
    return(NULL)

  data <- bind(requirements$data())
  explicit <- data[nzchar(data$Require) & nzchar(data$Version), ]
  if (nrow(explicit) == 0)
    return(NULL)

  # drop 'Dev' column
  explicit$Dev <- NULL

  # retrieve record version
  version <- record$Version
  if (is.null(version))
    return(NULL)

  # for each row, compute whether we're compatible
  rversion <- numeric_version(version)
  compatible <- map_lgl(seq_len(nrow(explicit)), function(i) {
    expr <- call(explicit$Require[[i]], rversion, explicit$Version[[i]])
    eval(expr, envir = baseenv())
  })

  # keep whatever wasn't compatible
  explicit[!compatible, ]

}

renv_retrieve_incompatible_report <- function(package, record, replacement, compat) {

  # only report if the user explicitly requesting installation of a particular
  # version of a package, but that package isn't actually compatible
  state <- renv_restore_state()
  if (!package %in% state$packages)
    return()

  fmt <- "%s (requires %s %s %s)"
  values <- with(compat, sprintf(fmt, Source, Package, Require, Version))

  fmt <- "Installation of '%s %s' was requested, but the following constraints are not met:"
  preamble <- with(record, sprintf(fmt, Package, Version))

  fmt <- "renv will try to install '%s %s' instead."
  postamble <- with(replacement, sprintf(fmt, Package, Version))

  if (!renv_tests_running()) {
    caution_bullets(
      preamble = preamble,
      values = values,
      postamble = postamble
    )
  }

}

renv_retrieve_origin <- function(host) {

  # NOTE: some host URLs may come with a protocol already formed;
  # if we find a protocol, use it as-is
  if (grepl("://", host, fixed = TRUE))
    return(host)

  # otherwise, prepend protocol (assume https)
  paste("https", host, sep = "://")

}
