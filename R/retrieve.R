
# this routine retrieves a package + its dependencies, and as a side
# effect populates the restore state's `retrieved` member with a
# list of package records which can later be used for install
renv_retrieve <- function(packages) {

  # confirm that we have restore state set up
  state <- renv_restore_state()
  if (is.null(state))
    stopf("renv_restore_begin() must be called first")

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

  # if the requested record is incompatible with the set
  # of requested package versions thus far, request the
  # latest version on the R package repositories
  #
  # TODO: handle more explicit dependency requirements
  # TODO: report to the user if they have explicitly requested
  # installation of this package version despite it being incompatible
  if (renv_retrieve_incompatible(record))
    record <- renv_available_packages_latest(package)

  if (!renv_restore_rebuild_required(record)) {

    # if we have an installed package matching the requested record, finish early
    path <- renv_restore_find(record)
    if (file.exists(path))
      return(renv_retrieve_successful(record, path, install = FALSE))

    # if the requested record already exists in the cache,
    # we'll use that package for install
    cacheable <-
      settings$use.cache(project = state$project) &&
      renv_record_cacheable(record)

    if (cacheable) {
      path <- renv_cache_package_path(record)
      if (renv_cache_package_validate(path))
        return(renv_retrieve_successful(record, path))
    }

  }

  # if this is a URL source, then it should already have a local path
  path <- record$Path %||% ""
  if (file.exists(path))
    return(renv_retrieve_successful(record, path))

  # if we find a suitable package tarball available locally,
  # then we can just use that directly (this also acts as an escape
  # hatch for cases where a package might have some known external source
  # but the user is unable to access that source in some context).
  #
  # TODO: consider if this should be guarded by a user preference
  retrieved <- catch(renv_retrieve_local(record))
  if (identical(retrieved, TRUE))
    return(TRUE)

  # if the user has provided an explicit path to a tarball in the source,
  # then just use that
  retrieved <- catch(renv_retrieve_explicit(record))
  if (identical(retrieved, TRUE))
    return(TRUE)

  # otherwise, try and restore from external source
  source <- tolower(record$Source %||% "unknown")
  if (source %in% c("git2r", "xgit"))
    source <- "git"

  # handle case where repository was previously declared as CRAN
  if (source == "cran")
    source <- "repository"

  switch(source,
         bioconductor = renv_retrieve_bioconductor(record),
         bitbucket    = renv_retrieve_bitbucket(record),
         git          = renv_retrieve_git(record),
         github       = renv_retrieve_github(record),
         gitlab       = renv_retrieve_gitlab(record),
         repository   = renv_retrieve_repos(record),
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
  package <- record$Package
  name <- renv_retrieve_name(record, type, ext)
  source <- tolower(record$Source)
  if (type == "source")
    renv_paths_source(source, package, name)
  else if (type == "binary")
    renv_paths_binary(source, package, name)
  else
    stopf("unrecognized type '%s'", type)
}

renv_retrieve_bioconductor <- function(record) {

  # ensure bioconductor support infrastructure initialized
  renv_bioconductor_init()

  # activate bioconductor repositories in this context
  repos <- getOption("repos")
  biocrepos <- c(renv_bioconductor_repos(), repos)
  options(repos = biocrepos[!duplicated(biocrepos)])
  on.exit(options(repos = repos), add = TRUE)

  # retrieve package as though from an active R repository
  renv_retrieve_repos(record)

}

renv_retrieve_bitbucket <- function(record) {

  origin <- renv_retrieve_origin(record$RemoteHost %||% "bitbucket.org")
  username <- record$RemoteUsername
  repo <- record$RemoteRepo
  sha <- record$RemoteSha %||% record$RemoteRef %||% "master"

  fmt <- "%s/%s/%s/get/%s.tar.gz"
  url <- sprintf(fmt, origin, username, repo, sha)
  path <- renv_retrieve_path(record)

  renv_retrieve_package(record, url, path)

}

renv_retrieve_github <- function(record) {

  origin <- renv_retrieve_origin(record$RemoteHost %||% "api.github.com")
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

  origin <- renv_retrieve_origin(record$RemoteHost %||% "gitlab.com")

  user <- record$RemoteUsername
  repo <- record$RemoteRepo
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)

  fmt <- "%s/api/v4/projects/%s/repository/archive.tar.gz"
  url <- sprintf(fmt, origin, id)
  path <- renv_retrieve_path(record)

  sha <- record$RemoteSha
  if (!is.null(sha))
    url <- paste(url, paste("sha", sha, sep = "="), sep = "?")

  renv_retrieve_package(record, url, path)

}

renv_retrieve_git <- function(record) {

  renv_git_preflight()

  package <- renv_tempfile("renv-git-")
  ensure_directory(package)

  template <- c(
    "cd \"${DIR}\"",
    "git init --quiet",
    "git remote add origin \"${ORIGIN}\"",
    "git fetch --quiet origin \"${REF}\"",
    "git reset --quiet --hard FETCH_HEAD"
  )

  data <- list(
    DIR    = renv_path_normalize(package),
    ORIGIN = record$RemoteUrl,
    REF    = record$RemoteSha %||% record$RemoteRef
  )

  commands <- renv_template_replace(template, data)
  command <- paste(commands, collapse = " && ")
  if (renv_platform_windows())
    command <- paste(comspec(), "/C", command)

  status <- local({

    renv_scope_auth(record)

    # use GIT_PAT when provided
    pat <- Sys.getenv("GIT_PAT", unset = NA)
    if (!is.na(pat)) {
      renv_scope_envvars(
        GIT_USERNAME = pat,
        GIT_PASSWORD = "x-oauth-basic"
      )
    }

    # set askpass helper
    # TODO: Windows?
    askpass <- system.file("resources/scripts-git-askpass.sh", package = "renv")
    renv_scope_envvars(GIT_ASKPASS = askpass)

    # run the command
    system(command)

  })

  if (status != 0L) {
    fmt <- "cannot retrieve package '%s' from '%s' [status code %i]"
    stopf(fmt, record$Package, record$RemoteUrl, status)
  }

  url <- paste("file://", package, sep = "")
  path <- renv_retrieve_path(record)
  renv_retrieve_package(record, url, path)

}

renv_retrieve_local_find <- function(record) {

  # packages installed with 'remotes::install_local()' will
  # have a RemoteUrl entry that we can use
  url <- record$RemoteUrl %||% ""
  if (file.exists(url)) {
    path <- renv_path_normalize(url, winslash = "/", mustWork = TRUE)
    type <- if (fileext(path) %in% c(".tgz", ".zip")) "binary" else "source"
    return(named(path, type))
  }

  # otherwise, use our own local cache of packages
  roots <- c(
    renv_paths_project("renv/local"),
    renv_paths_local()
  )

  for (type in c("binary", "source")) {
    name <- renv_retrieve_name(record, type = type)
    for (root in roots) {
      path <- file.path(root, record$Package, name)
      if (file.exists(path))
        return(named(path, type))
    }
  }

  fmt <- "%s [%s] is not available locally"
  stopf(fmt, record$Package, record$Version)

}

renv_retrieve_local_report <- function(record) {

  source <- tolower(record$Source)
  if (tolower(source) == "local")
    return(record)

  record$Source <- "local"
  rather <- if (source == "unknown") "" else paste(" rather than", renv_alias(source))
  fmt <- "* Package %s [%s] will be installed from local sources%s."
  with(record, vwritef(fmt, Package, Version, rather))

  record

}

renv_retrieve_local <- function(record) {
  source <- renv_retrieve_local_find(record)
  record <- renv_retrieve_local_report(record)
  renv_retrieve_successful(record, source)
}

renv_retrieve_explicit <- function(record) {

  # try parsing as a local remote
  source <- record$Source %||% ""
  record <- catch(renv_remotes_resolve_local(source))
  if (inherits(record, "error"))
    return(FALSE)

  # treat as 'local' source but extract path
  normalized <- renv_path_normalize(source, winslash = "/", mustWork = TRUE)
  record$Source <- "local"
  renv_retrieve_successful(record, normalized)

}

renv_retrieve_repos <- function(record) {

  # if the record doesn't declare the package version,
  # treat it as a request for the latest version on CRAN
  # TODO: should make this behavior configurable
  if (is.null(record$Version))
    record <- renv_available_packages_latest(record$Package)

  # if this record is tagged with a type + url, we can
  # use that directly for retrieval
  if (all(c("type", "url") %in% names(attributes(record))))
    return(renv_retrieve_repos_impl(record))

  # always attempt to retrieve from source + archive
  methods <- c(
    renv_retrieve_repos_source,
    renv_retrieve_repos_archive
  )

  # only attempt to retrieve binaries when explicitly requested by user
  # TODO: what about binaries on Linux?
  if (!identical(getOption("pkgType"), "source"))
    methods <- c(renv_retrieve_repos_binary, methods)

  for (method in methods) {
    status <- method(record)
    if (identical(status, TRUE))
      return(TRUE)
  }

  stopf("failed to retrieve package '%s'", record$Package)

}

renv_retrieve_repos_archive_name <- function(record, type) {
  fmt <- "%s_%s%s"
  sprintf(fmt, record$Package, record$Version, renv_package_ext(type))
}

renv_retrieve_repos_binary <- function(record) {
  renv_retrieve_repos_impl(record, "binary")

}

renv_retrieve_repos_source <- function(record) {
  renv_retrieve_repos_impl(record, "source")
}

renv_retrieve_repos_archive <- function(record) {

  name <- sprintf("%s_%s.tar.gz", record$Package, record$Version)
  for (repo in getOption("repos")) {
    repo <- file.path(repo, "src/contrib/Archive", record$Package)
    status <- catch(renv_retrieve_repos_impl(record, "source", name, repo))
    if (identical(status, TRUE))
      return(TRUE)
  }

  return(FALSE)

}

renv_retrieve_repos_impl <- function(record,
                                     type = NULL,
                                     name = NULL,
                                     repo = NULL)
{
  type <- type %||% attr(record, "type", exact = TRUE)
  name <- name %||% renv_retrieve_repos_archive_name(record, type)
  repo <- repo %||% attr(record, "url", exact = TRUE)

  # if we weren't provided a repository for this package, try to find it
  if (is.null(repo)) {

    entry <- catch(
      renv_available_packages_entry(
        package = record$Package,
        type    = type,
        filter  = record$Version
      )
    )

    if (inherits(entry, "error"))
      return(FALSE)

    # add in the path if available
    repo <- entry$Repository
    if (!is.null(entry$Path) && !is.na(entry$Path))
      repo <- file.path(repo, entry$Path)

  }

  url <- file.path(repo, name)
  path <- renv_retrieve_path(record, type)

  renv_retrieve_package(record, url, path)

}


renv_retrieve_package <- function(record, url, path) {

  ensure_parent_directory(path)
  type <- tolower(record$Source)
  status <- local({
    renv_scope_auth(record)
    catch(download(url, destfile = path, type = type))
  })

  if (inherits(status, "error") || identical(status, FALSE))
    return(status)

  renv_retrieve_successful(record, path)

}

renv_retrieve_successful <- function(record, path, install = TRUE) {

  # augment record with information from DESCRIPTION file
  subdir <- record$RemoteSubdir
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
  deps <- renv_dependencies_discover_description(path)
  rowapply(deps, function(dep) {
    package <- dep$Package
    requirements[[package]] <- requirements[[package]] %||% stack()
    requirements[[package]]$push(dep)
  })

  # read and handle remotes declared by this package
  renv_retrieve_handle_remotes(record)

  # ensure its dependencies are retrieved as well
  if (state$recursive)
    for (package in unique(deps$Package))
      renv_retrieve(package)

  # mark package as requiring install if needed
  if (install)
    state$install$push(record)

  TRUE

}

renv_retrieve_unknown_source <- function(record) {

  # try to find a matching local package
  status <- catch(renv_retrieve_local(record))
  if (!inherits(status, "error"))
    return(status)

  # failed; parse as though from R package repository
  record$Source <- "Repository"
  renv_retrieve_repos(record)

}

renv_retrieve_handle_remotes <- function(record) {

  # TODO: what should we do if we detect incompatible remotes?
  # e.g. if pkg A requests 'r-lib/rlang@0.3' but pkg B requests
  # 'r-lib/rlang@0.2'.

  # check and see if this package declares Remotes -- if so,
  # use those to fill in any missing records
  desc <- renv_description_read(record$Path)
  if (is.null(desc$Remotes))
    return(NULL)

  fields <- strsplit(desc$Remotes, "\\s*,\\s*")[[1]]
  for (field in fields) {

    # TODO: allow customization of behavior when remote parsing fails?
    remote <- catch(renv_remotes_resolve(field))
    if (inherits(remote, "error")) {
      fmt <- "failed to parse remote '%s' declared by package '%s'; skipping"
      warningf(fmt, field, record$Package)
      next
    }


    # if installation of this package was not specifically requested by
    # the user (ie: it's been requested as it's a dependency of this package)
    # then update the record. note that we don't want to update in explicit
    # installs as we don't want to override what was reported / requested
    # in e.g. `renv::restore()`
    state <- renv_restore_state()
    if (remote$Package %in% state$packages)
      next

    records <- state$records
    records[[remote$Package]] <- remote
    state$records <- records

  }

}

renv_retrieve_missing_record <- function(package) {

  # TODO: allow users to configure the action to take here, e.g.
  #
  #   1. retrieve latest from R repositories (the default),
  #   2. request a package + version to be retrieved,
  #   3. hard error
  #
  renv_available_packages_latest(package)

}

# check to see if this requested record is incompatible
# with the set of required dependencies recorded thus far
# during the package retrieval process
renv_retrieve_incompatible <- function(record) {

  state <- renv_restore_state()

  # check and see if the installed version satisfies all requirements
  requirements <- state$requirements[[record$Package]]
  if (is.null(requirements))
    return(FALSE)

  data <- bind_list(requirements$data())
  explicit <- data[nzchar(data$Require) & nzchar(data$Version), ]
  if (nrow(explicit) == 0)
    return(FALSE)

  expr <- c(
    sprintf("version <- numeric_version('%s')", record$Version),
    paste(
      sprintf("version %s '%s'", explicit$Require, explicit$Version),
      collapse = " && "
    )
  )

  envir <- new.env(parent = baseenv())
  satisfied <- catch(eval(parse(text = expr), envir = envir))
  if (inherits(satisfied, "error"))
    warning(satisfied)

  !identical(satisfied, TRUE)

}

renv_retrieve_origin <- function(host) {

  # NOTE: some host URLs may come with a protocol already formed;
  # if we find a protocol, use it as-is
  if (grepl("://", host, fixed = TRUE))
    return(host)

  # otherwise, prepend protocol (assume https)
  paste("https", host, sep = "://")

}
