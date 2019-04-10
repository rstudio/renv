
# this routine retrieves a package + its dependencies, and as a side
# effect populates the restore state's `retrieved` member with a
# list of package records which can later be used for install
renv_retrieve <- function(packages) {

  # confirm that we have restore state set up
  state <- renv_restore_state()
  if (is.null(state))
    stopf("renv_restore_begin() must be called first")

  # cache set of installed packages (avoid re-querying on each retrieval)
  renv_global_set("installed.packages", renv_installed_packages())
  on.exit(renv_global_clear("installed.packages"), add = TRUE)

  # TODO: parallel?
  for (package in packages)
    renv_retrieve_impl(package)

  state <- renv_restore_state()
  data <- state$retrieved$data()
  names(data) <- extract_chr(data, "Package")
  data

}

renv_retrieve_impl <- function(package) {

  # skip packages with 'base' priority
  if (renv_package_priority(package) == "base")
    return()

  # if we've already attempted retrieval of this package, skip
  state <- renv_restore_state()
  if (visited(package, envir = state$retrieved.env))
    return()

  # extract record for package
  records <- state$records
  record <- records[[package]] %||% renv_retrieve_missing_record(package)

  # if the requested record is incompatible with the set
  # of requested package versions thus far, request the
  # latest version on CRAN
  #
  # TODO: handle more explicit dependency requirements
  # TODO: report to the user if they have explicitly requested
  # installation of this package version despite it being incompatible
  if (renv_retrieve_incompatible(record))
    record <- renv_retrieve_missing_record(package)

  # if the package is otherwise skippable, skip it
  if (renv_restore_skip(record))
    return()

  # if the requested record already exists in the cache, we can finish early
  path <- renv_cache_package_path(record)
  if (file.exists(path))
    return(renv_retrieve_successful(record, path))

  # otherwise, try and restore from external source
  source <- tolower(record[["Source"]] %||% record[["RemoteType"]] %||% "CRAN")
  switch(source,
         cran         = renv_retrieve_cran(record),
         bioconductor = renv_retrieve_bioconductor(record),
         bitbucket    = renv_retrieve_bitbucket(record),
         git          = renv_retrieve_git(record),
         git2r        = renv_retrieve_git(record),
         github       = renv_retrieve_github(record),
         gitlab       = renv_retrieve_gitlab(record),
         local        = renv_retrieve_local(record),
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
  source <- tolower(record$Source %||% record$RemoteType %||% "")
  if (type == "source")
    renv_paths_source(package, source, name)
  else if (type == "binary")
    renv_paths_binary(package, source, name)
  else
    stopf("unrecognized type '%s'", type)
}

renv_retrieve_bioconductor <- function(record) {

  # activate bioconductor repositories in this context
  repos <- getOption("repos")
  options(repos = unique(c(renv_bioconductor_repos(), repos)))
  on.exit(options(repos = repos), add = TRUE)

  renv_retrieve_cran(record)
}

renv_retrieve_bitbucket <- function(record) {

  host <- record$RemoteHost %||% "bitbucket.org"
  sha <- record$RemoteSha %||% record$RemoteRef %||% "master"

  fmt <- "https://%s/%s/%s/get/%s.tar.gz"
  url <- sprintf(fmt, host, record$RemoteUsername, record$RemoteRepo, sha)
  path <- renv_retrieve_path(record)

  renv_retrieve_package(record, url, path)

}

renv_retrieve_github <- function(record) {

  record$RemoteHost <- record$RemoteHost %||% "api.github.com"

  fmt <- "https://%s/repos/%s/%s/tarball/%s"

  ref <- record$RemoteSha %||% record$RemoteRef
  if (is.null(ref)) {
    fmt <- "GitHub record for package '%s' has no recorded 'RemoteSha' / 'RemoteRef'"
    stopf(fmt, record$Package)
  }

  url <- with(record, sprintf(fmt, RemoteHost, RemoteUsername, RemoteRepo, ref))
  path <- renv_retrieve_path(record)
  renv_retrieve_package(record, url, path)

}

renv_retrieve_gitlab <- function(record) {

  # TODO: remotes doesn't appear to understand how to interact with GitLab API?
  host <- record$RemoteHost %||% "gitlab.com"
  id <- paste(record$RemoteUsername, record$RemoteRepo, sep = "%2F")

  fmt <- "https://%s/api/v4/projects/%s/repository/archive.tar.gz"
  url <- sprintf(fmt, host, id)
  path <- renv_retrieve_path(record)

  sha <- record$RemoteSha
  if (!is.null(sha))
    url <- paste(url, paste("sha", sha, sep = "="), sep = "?")

  renv_retrieve_package(record, url, path)

}

renv_retrieve_git <- function(record) {

  remotes <- renv_retrieve_require("remotes", "Git")

  remote <- remotes$git_remote(
    url = record$RemoteUrl,
    ref = record$RemoteSha %||% record$RemoteRef
  )

  renv_retrieve_remote(record, remote, remotes)

}

renv_retrieve_local_find <- function(record) {

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

renv_retrieve_local <- function(record) {
  source <- renv_retrieve_local_find(record)
  url <- paste("file://", source, sep = "")
  path <- renv_retrieve_path(record, type = names(source))
  renv_retrieve_package(record, url, path)
}

renv_retrieve_cran <- function(record) {

  # if the record doesn't declare the package version,
  # treat it as a request for the latest version on CRAN
  # TODO: should make this behavior configurable
  if (is.null(record$Version))
    record <- renv_retrieve_missing_record(record$Package)

  # if we already have a type + repository, no need to find it
  if (!is.null(record$Type) && !is.null(record$Repository))
    return(renv_retrieve_cran_impl(record))

  # always attempt to retrieve from source + archive
  methods <- c(
    renv_retrieve_cran_source,
    renv_retrieve_cran_archive
  )

  # only attempt to retrieve binaries when explicitly requested by user
  # TODO: what about binaries on Linux?
  if (!identical(getOption("pkgType"), "source"))
    methods <- c(renv_retrieve_cran_binary, methods)

  for (method in methods) {
    status <- method(record)
    if (identical(status, TRUE))
      return(TRUE)
  }

  stopf("failed to retrieve package '%s' from CRAN", record$Package)

}

renv_retrieve_cran_archive_name <- function(record, type) {
  fmt <- "%s_%s%s"
  sprintf(fmt, record$Package, record$Version, renv_package_ext(type))
}

renv_retrieve_cran_binary <- function(record) {
  renv_retrieve_cran_impl(record, "binary")

}

renv_retrieve_cran_source <- function(record) {
  renv_retrieve_cran_impl(record, "source")
}

renv_retrieve_cran_archive <- function(record) {

  name <- sprintf("%s_%s.tar.gz", record$Package, record$Version)
  for (repo in getOption("repos")) {
    repo <- file.path(repo, "src/contrib/Archive", record$Package)
    status <- catch(renv_retrieve_cran_impl(record, "source", name, repo))
    if (identical(status, TRUE))
      return(TRUE)
  }

  return(FALSE)

}

renv_retrieve_cran_impl <- function(record,
                                    type = NULL,
                                    name = NULL,
                                    repo = NULL)
{
  type <- type %||% record$Type
  name <- name %||% renv_retrieve_cran_archive_name(record, type)
  repo <- repo %||% record$Repository

  # if we weren't provided a repository for this package, try to find it
  if (is.null(repo)) {
    filter <- function(entry) identical(record$Version, entry$Version)
    entry <- catch(renv_available_packages_entry(record$Package, type, filter))
    if (inherits(entry, "error"))
      return(FALSE)
    repo <- entry$Repository
  }

  url <- file.path(repo, name)
  path <- renv_retrieve_path(record, type)

  renv_retrieve_package(record, url, path)

}


renv_retrieve_package <- function(record, url, path) {

  # download the package
  # TODO: validate that the existing tarball / zipball is not damaged
  ensure_parent_directory(path)
  type <- record$RemoteType %||% record$Source
  status <- catch(download(url, destfile = path, type = type))
  if (inherits(status, "error") || identical(status, FALSE))
    return(status)

  renv_retrieve_successful(record, path)

}

renv_retrieve_successful <- function(record, path) {

  # augment record with information from DESCRIPTION file
  desc <- renv_description_read(path)
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
    requirements[[package]] <<- requirements[[package]] %||% stack()
    requirements[[package]]$push(dep)
  })

  # read and handle remotes declared by this package
  renv_retrieve_handle_remotes(record)

  # ensure its dependencies are retrieved as well
  if (state$recursive)
    for (package in unique(deps$Package))
      renv_retrieve(package)

  # record package as retrieved
  state$retrieved$push(record)

  TRUE

}

renv_retrieve_unknown_source <- function(record) {

  status <- renv_retrieve_local(record)
  if (!inherits(status, "error"))
    return(status)

  record <- renv_retrieve_missing_record(record$Package)
  renv_retrieve_cran(record)
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
    remote <- catch(renv_remotes_parse(field))
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
  #   1. retrieve latest from CRAN (the default),
  #   2. request a package + version to be retrieved,
  #   3. hard error
  #
  types <- if (Sys.info()[["sysname"]] == "Linux")
    "source"
  else
    c("binary", "source")

  # iterate through available packages reported by all repositories
  # and look for a matching entry
  entries <- bapply(types, function(type) {

    entry <- catch(renv_available_packages_entry(package, type))
    if (inherits(entry, "error"))
      return(NULL)

    c(entry[c("Package", "Version", "Repository")], Type = type)

  })

  if (!is.data.frame(entries)) {
    fmt <- "failed to retrieve package '%s' (missing record)"
    stopf(fmt, package)
  }

  # since multiple entries could match, take the newest version by default
  # TODO: could also allow older binary version here
  idx <- with(entries, order(Version, factor(Type, c("source", "binary"))))
  entry <- entries[tail(idx, n = 1), ]

  list(
    Package    = package,
    Version    = entry$Version,
    Source     = "CRAN",
    Type       = entry$Type,
    Repository = entry$Repository
  )

}

renv_retrieve_require <- function(package, source) {

  if (requireNamespace(package, quietly = TRUE))
    return(.getNamespace(package))

  fmt <- "package '%s' is required to retrieve packages from %s"
  stopf(fmt, package, source)

}

renv_retrieve_remote <- function(record, remote, remotes) {

  # download the remote
  start <- Sys.time()
  vwritef("Retrieving '%s' ...", record$Package)
  bundle <- remotes$remote_download(remote, quiet = TRUE)
  on.exit(unlink(bundle), add = TRUE)
  renv_download_report(Sys.time() - start, file.size(bundle))

  # augment with remote information
  source <- remotes$source_pkg(bundle, subdir = remote$subdir)
  on.exit(unlink(source, recursive = TRUE), add = TRUE)

  remotes$update_submodules(source, TRUE)
  remotes$add_metadata(source, remotes$remote_metadata(remote, bundle, source, record$RemoteSha))
  remotes$clear_description_md5(source)

  # re-pack the package
  owd <- setwd(dirname(source))
  on.exit(setwd(owd), add = TRUE)
  tarfile <- sprintf("%s.tar.gz", source)
  tar(tarfile = tarfile, files = basename(source), compression = "gzip")
  setwd(owd)

  # construct path to package
  url <- paste("file://", tarfile, sep = "")

  # construct nice name for cache
  path <- renv_retrieve_path(record)
  ensure_parent_directory(path)

  renv_retrieve_package(record, url, path)

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

  exprs <- sprintf(
    "numeric_version('%s') %s '%s'",
    record$Version,
    explicit$Require,
    explicit$Version
  )

  expr <- paste(exprs, collapse = " && ")
  envir <- new.env(parent = baseenv())
  satisfied <- catch(eval(parse(text = expr), envir = envir))
  if (inherits(satisfied, "error"))
    warning(satisfied)

  !identical(satisfied, TRUE)

}
