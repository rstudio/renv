
`_renv_repos_archive` <- new.env(parent = emptyenv())

# this routine retrieves a package + its dependencies, and as a side
# effect populates the restore state's `retrieved` member with a
# list of package records which can later be used for install
retrieve <- function(packages) {

  # confirm that we have restore state set up
  state <- renv_restore_state()
  if (is.null(state))
    stopf("renv_restore_begin() must be called first")

  # normalize repositories (ensure @CRAN@ is resolved)
  options(repos = renv_repos_normalize())

  # transform repository URLs for RSPM
  if (renv_rspm_enabled()) {
    repos <- getOption("repos")
    renv_scope_options(repos = renv_rspm_transform(repos))
  }

  # ensure HTTPUserAgent is set (required for RSPM binaries)
  agent <- renv_http_useragent()
  if (!grepl("renv", agent)) {
    renv <- sprintf("renv (%s)", renv_package_version("renv"))
    agent <- paste(renv, agent, sep = "; ")
  }
  renv_scope_options(HTTPUserAgent = agent)

  # TODO: parallel?
  handler <- state$handler
  for (package in packages)
    handler(package, renv_retrieve_impl(package))

  state <- renv_restore_state()
  data <- state$install$data()
  names(data) <- extract_chr(data, "Package")
  data

}

renv_retrieve_impl <- function(package) {

  # skip packages with 'base' priority
  if (package %in% renv_packages_base())
    return()

  # if we've already attempted retrieval of this package, skip
  state <- renv_restore_state()
  if (visited(package, envir = state$retrieved))
    return()

  # extract record for package
  records <- state$records
  record <- records[[package]] %||% renv_retrieve_missing_record(package)

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
    record <- renv_available_packages_latest(package)
    if (is.null(record)) {
      stopf("package '%s' is not available", package)
      return()
    }
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

  if (!renv_restore_rebuild_required(record)) {

    # if we have an installed package matching the requested record, finish early
    path <- renv_restore_find(package, record)
    if (file.exists(path))
      return(renv_retrieve_successful(record, path, install = FALSE))

    # if the requested record already exists in the cache,
    # we'll use that package for install
    cacheable <-
      renv_cache_config_enabled(project = state$project) &&
      renv_record_cacheable(record)

    if (cacheable) {

      # try to find the record in the cache
      path <- renv_cache_find(record)
      if (renv_cache_package_validate(path))
        return(renv_retrieve_successful(record, path))
    }

  }

  # if this is a URL source, then it should already have a local path
  path <- record$Path %||% record$Source %||% ""
  if (grepl("[/\\]", path) && file.exists(path)) {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    return(renv_retrieve_successful(record, path))
  }

  if (!renv_restore_rebuild_required(record)) {

    # try some early shortcut methods
    shortcuts <- c(
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

  }

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
  source <- renv_record_source(record)

  # check for packages from an RSPM binary URL, and
  # update the package type if known
  if (renv_rspm_enabled()) {
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

  template <- heredoc('
    cd "${DIR}"
    git init --quiet
    git remote add origin "${ORIGIN}"
    git fetch --quiet origin "${REF}"
    git reset --quiet --hard FETCH_HEAD
  ')

  data <- list(
    DIR    = renv_path_normalize(path),
    ORIGIN = url,
    REF    = sha %||% ref
  )

  commands <- renv_template_replace(template, data)
  command <- gsub("\n", " && ", commands, fixed = TRUE)
  if (renv_platform_windows())
    command <- paste(comspec(), "/C", command)

  vwritef("Cloning '%s' ...", url)

  before <- Sys.time()

  status <- local({
    renv_scope_auth(record)
    renv_scope_git_auth()
    system(command)
  })

  after <- Sys.time()

  if (status != 0L) {
    fmt <- "error cloning '%s' from '%s' [status code %i]"
    stopf(fmt, package, url, status)
  }

  fmt <- "\tOK [cloned repository in %s]"
  vwritef(fmt, renv_difftime_format(after - before))

  TRUE

}


renv_retrieve_cellar_find <- function(record, project = NULL) {

  project <- renv_project_resolve(project)

  # packages installed with 'remotes::install_local()' will
  # have a RemoteUrl entry that we can use
  url <- record$RemoteUrl %||% ""
  if (file.exists(url)) {
    path <- renv_path_normalize(url, winslash = "/", mustWork = TRUE)
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

  fmt <- "* Package %s [%s] will be installed from the cellar."
  with(record, vwritef(fmt, Package, Version))

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
  resolved <- catch(renv_remotes_resolve_path(source))
  if (inherits(resolved, "error"))
    return(FALSE)

  # treat as 'local' source but extract path
  normalized <- renv_path_normalize(source, winslash = "/", mustWork = TRUE)
  resolved$Source <- "Local"
  renv_retrieve_successful(resolved, normalized)

}

renv_retrieve_repos <- function(record) {

  # if this record is tagged with a type + url, we can
  # use that directly for retrieval
  if (all(c("type", "url") %in% names(attributes(record))))
    return(renv_retrieve_repos_impl(record))

  # collect list of 'methods' for retrieval
  methods <- stack(mode = "list")

  # only attempt to retrieve binaries when explicitly requested by user
  if (!identical(getOption("pkgType"), "source")) {

    # prefer repository binaries if available
    methods$push(renv_retrieve_repos_binary)

    # if MRAN is enabled, check those binaries as well
    if (config$mran.enabled())
      methods$push(renv_retrieve_repos_mran)

  }

  # next, try to retrieve from sources
  methods$push(renv_retrieve_repos_source)

  # if this is a package from r-universe, try restoring from github
  # (currently inferred from presence for RemoteUrl field)
  unifields <- c("RemoteUrl", "RemoteRef", "RemoteSha")
  if (all(unifields %in% names(record)))
    methods$push(renv_retrieve_git)
  else
    methods$push(renv_retrieve_repos_archive)

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
      warning(status)
      next
    }

    if (identical(status, TRUE))
      return(TRUE)

    if (!is.logical(status)) {
      fmt <- "internal error: unexpected status code '%s'"
      warningf(fmt, renv_deparse(status))
    }

  }

  # if we couldn't download the package, report the errors we saw
  local({
    renv_scope_options(warn = 1)
    for (error in errors$data())
      warning(error)
  })

  stopf("failed to retrieve package '%s'", renv_record_format_remote(record))

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

  renv_pretty_print(
    values   = paste("-", messages),
    preamble = preamble,
    wrap     = FALSE
  )

  if (renv_tests_running() && renv_tests_verbose())
    str(errors)

}

renv_retrieve_url <- function(record) {

  if (is.null(record$RemoteUrl)) {
    fmt <- "package '%s' has no recorded RemoteUrl"
    stopf(fmt, record$Package)
  }

  resolved <- renv_remotes_resolve_url(record$RemoteUrl, quiet = FALSE)
  renv_retrieve_successful(record, resolved$Path)

}

renv_retrieve_repos_archive_name <- function(record, type = "source") {
  record$File %||% {
    ext <- renv_package_ext(type)
    paste0(record$Package, "_", record$Version, ext)
  }
}

renv_retrieve_repos_mran <- function(record) {

  # MRAN does not make binaries available on Linux
  if (renv_platform_linux())
    return(FALSE)

  # ensure local MRAN database is up-to-date
  renv_mran_database_refresh(explicit = FALSE)

  # check that we have an available database
  path <- renv_mran_database_path()
  if (!file.exists(path))
    return(FALSE)

  # attempt to read it
  database <- catch(renv_mran_database_load())
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
  base <- renv_mran_url(date, suffix)
  name <- renv_retrieve_name(record, type = "binary")
  url <- file.path(base, name)

  # form path to saved file
  path <- renv_retrieve_path(record, "binary")

  # attempt to retrieve
  renv_retrieve_package(record, url, path)

}

renv_retrieve_repos_binary <- function(record) {
  renv_retrieve_repos_impl(record, "binary")
}

renv_retrieve_repos_source <- function(record) {
  renv_retrieve_repos_impl(record, "source")
}

renv_retrieve_repos_archive <- function(record) {

  for (repo in getOption("repos")) {

    # try to determine path to package in archive
    url <- renv_retrieve_repos_archive_path(repo, record)
    if (is.null(url))
      next

    # attempt download
    name <- renv_retrieve_repos_archive_name(record, type = "source")
    status <- catch(renv_retrieve_repos_impl(record, "source", name, url))
    if (identical(status, TRUE))
      return(TRUE)

  }

  return(FALSE)

}

renv_retrieve_repos_archive_path <- function(repo, record) {

  # allow users to provide a custom archive path for a record,
  # in case they're using a repository that happens to archive
  # packages with a different format than regular CRAN network
  # https://github.com/rstudio/renv/issues/602
  override <- getOption("renv.retrieve.repos.archive.path")
  if (is.function(override)) {
    result <- override(repo, record)
    if (!is.null(result))
      return(result)
  }

  # if we already know the format of the repository, use that
  if (exists(repo, envir = `_renv_repos_archive`)) {
    formatter <- get(repo, envir = `_renv_repos_archive`)
    root <- formatter(repo, record)
    return(root)
  }

  # otherwise, try determining the archive paths with a couple
  # custom locations, and cache the version that works for the
  # associated repository
  formatters <- list(

    # default CRAN format
    function(repo, record) {
      with(record, file.path(repo, "src/contrib/Archive", Package))
    },

    # format used by Artifactory
    # https://github.com/rstudio/renv/issues/602
    function(repo, record) {
      with(record, file.path(repo, "src/contrib/Archive", Package, Version))
    },

    # format used by Nexus
    # https://github.com/rstudio/renv/issues/595
    function(repo, record) {
      with(record, file.path(repo, "src/contrib"))
    }

  )

  name <- renv_retrieve_repos_archive_name(record, "source")
  for (formatter in formatters) {
    root <- formatter(repo, record)
    url <- file.path(root, name)
    if (renv_download_available(url)) {
      assign(repo, formatter, envir = `_renv_repos_archive`)
      return(root)
    }
  }

}

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

    # add in the path if available
    repo <- entry$Repository
    if (!is.null(entry$Path) && !is.na(entry$Path))
      repo <- file.path(repo, entry$Path)

    # update the tarball name if it was declared
    file <- entry$File
    if (!is.null(file))
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
    catch(download(url, destfile = path, type = type))
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
  record$Package <- desc$Package
  record$Version <- desc$Version

  # add in path information to record (used later during install)
  record$Path <- path

  # record this package's requirements
  state <- renv_restore_state()
  requirements <- state$requirements
  deps <- renv_dependencies_discover_description(path, subdir = subdir)
  if (length(deps$Source))
    deps$Source <- record$Package

  rowapply(deps, function(dep) {
    package <- dep$Package
    requirements[[package]] <- requirements[[package]] %||% stack()
    requirements[[package]]$push(dep)
  })

  # read and handle remotes declared by this package
  renv_retrieve_handle_remotes(record, subdir = subdir)

  # ensure its dependencies are retrieved as well
  if (state$recursive)
    for (package in unique(deps$Package))
      retrieve(package)

  # mark package as requiring install if needed
  if (install)
    state$install$push(record)

  TRUE

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

renv_retrieve_handle_remotes <- function(record, subdir) {

  # TODO: what should we do if we detect incompatible remotes?
  # e.g. if pkg A requests 'r-lib/rlang@0.3' but pkg B requests
  # 'r-lib/rlang@0.2'.

  # check and see if this package declares Remotes -- if so,
  # use those to fill in any missing records
  path <- record$Path
  desc <- renv_description_read(path = path, subdir = subdir)
  if (is.null(desc$Remotes))
    return(NULL)

  fields <- strsplit(desc$Remotes, "\\s*,\\s*")[[1]]
  for (field in fields) {

    # TODO: allow customization of behavior when remote parsing fails?
    remote <- catch(renv_remotes_resolve(field))
    if (inherits(remote, "error")) {
      fmt <- "failed to resolve remote '%s' declared by package '%s'; skipping"
      warningf(fmt, field, record$Package)
      next
    }


    # if installation of this package was not specifically requested by
    # the user (ie: it's been requested as it's a dependency of this package)
    # then update the record. note that we don't want to update in explicit
    # installs as we don't want to override what was reported / requested
    # in e.g. `renv::restore()`.
    #
    # allow override if a non-specific version of the package was requested
    package <- remote$Package
    state <- renv_restore_state()
    if (package %in% state$packages) {
      record <- state$records[[package]]
      if (!identical(record, list(Package = package, Source = "Repository")))
        next
    }

    # only update the record if we don't have an existing instance
    # of the record. the intention here is that remotes specified in,
    # say, the project DESCRIPTION file should take precedence over
    # remotes defined by packages themselves. there is some obvious
    # potential for breakage here though
    state$records[[package]] <- state$records[[package]] %||% remote

  }

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

  msg <- sprintf(fmt, package, version)
  writeLines(msg)

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
    renv_pretty_print(
      values = values,
      preamble = preamble,
      postamble = postamble,
      wrap = FALSE
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
