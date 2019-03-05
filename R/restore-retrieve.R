
# this routine retrieves a package + its dependencies, and as a side
# effect populates the restore state's `retrieved` member with a
# list of package records which can later be used for install
renv_restore_retrieve <- function(packages, lockfile = NULL) {

  # TODO: parallel?
  for (package in packages)
    renv_restore_retrieve_impl(package, lockfile)

  state <- renv_restore_state()
  return(state$records$data())

}

renv_restore_retrieve_impl <- function(package, lockfile) {

  # skip 'R' package that might be passed in here
  if (package == "R")
    return()

  # skip 'base' packages
  base <- renv_installed_packages_base()
  if (identical(base[package, "Priority"], "base"))
    return()

  # if we've already attempted retrieval of this package, skip
  state <- renv_restore_state()
  if (exists(package, envir = state$retrieved))
    return()

  assign(package, TRUE, envir = state$retrieved)

  # extract lockfile if none provided
  lockfile <- lockfile %||% state$lockfile

  # extract the package record (attempt to construct one if missing)
  record <- lockfile$R$Package[[package]]

  # if we don't have a package record, try to infer one for retrieval
  record <- record %||% renv_restore_missing_record(package)

  # otherwise, try and restore from external source
  # TODO: what to assume if no source provided? just use CRAN?
  source <- tolower(record[["Source"]] %||% "CRAN")

  # TODO: how should packages from an unknown source be handled?

  switch(source,
    cran         = renv_restore_retrieve_cran(record),
    bioconductor = renv_restore_retrieve_bioconductor(record),
    github       = renv_restore_retrieve_github(record),
    bitbucket    = renv_restore_retrieve_bitbucket(record),
    renv_restore_retrieve_unknown_source(record)
  )

}

renv_restore_missing_record <- function(package) {

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

renv_restore_retrieve_bioconductor <- function(record) {
  repos <- renv_bioconductor_repos()

  # activate bioconductor repositories in this context
  repos <- getOption("repos")
  options(repos = unique(c(renv_bioconductor_repos(), repos)))
  on.exit(options(repos = repos), add = TRUE)

  renv_restore_retrieve_cran(record)
}

renv_restore_retrieve_github <- function(record) {

  # TODO: use remotes? or use httr (so we can auth)?
  fmt <- "https://%s/repos/%s/%s/tarball/%s"

  url <- with(record, sprintf(fmt, RemoteHost, RemoteUsername, RemoteRepo, RemoteSha))
  path <- renv_paths_source(record$Package, record$RemoteSha)

  renv_restore_retrieve_package(record, url, path, "source")

}

renv_restore_retrieve_bitbucket <- function(record) {
  # TODO
}

renv_restore_retrieve_cran <- function(record) {

  # if we already have a type + repository, no need to find it
  if (!is.null(record$Type) && !is.null(record$Repository))
    return(renv_restore_retrieve_cran_impl(record))

  # always attempt to retrieve from source + archive
  methods <- c(
    renv_restore_retrieve_cran_source,
    renv_restore_retrieve_cran_archive
  )

  # only attempt to retrieve binaries when explicitly requested by user
  # TODO: what about binaries on Linux?
  if (!identical(getOption("pkgType"), "source"))
    methods <- c(renv_restore_retrieve_cran_binary, methods)

  for (method in methods) {
    status <- method(record)
    if (inherits(status, "error"))
      stop(status)

    if (identical(status, TRUE))
      return(TRUE)
  }

  stopf("failed to retrieve package '%s' from CRAN", record$Package)

}

renv_restore_retrieve_cran_archive_name_binary <- function(record) {
  sysname <- Sys.info()[["sysname"]]
  suffix <- switch(sysname, Darwin = "tgz", Windows = "zip", "tar.gz")
  sprintf("%s_%s.%s", record$Package, record$Version, suffix)
}

renv_restore_retrieve_cran_archive_name_source <- function(record) {
  sprintf("%s_%s.tar.gz", record$Package, record$Version)
}

renv_restore_retrieve_cran_archive_name <- function(record, type) {
  case(
    type == "binary" ~ renv_restore_retrieve_cran_archive_name_binary(record),
    type == "source" ~ renv_restore_retrieve_cran_archive_name_source(record)
  )
}

renv_restore_retrieve_cran_binary <- function(record) {
  renv_restore_retrieve_cran_impl(record, "binary")

}

renv_restore_retrieve_cran_source <- function(record) {
  renv_restore_retrieve_cran_impl(record, "source")
}

renv_restore_retrieve_cran_archive <- function(record) {

  name <- sprintf("%s_%s.tar.gz", record$Package, record$Version)
  for (repo in getOption("repos")) {
    repo <- file.path(repo, "src/contrib/Archive", record$Package)
    status <- catch(renv_restore_retrieve_cran_impl(record, "source", name, repo))
    if (identical(status, TRUE))
      return(TRUE)
  }

  return(FALSE)

}

renv_restore_retrieve_cran_impl <- function(record,
                                           type = NULL,
                                           name = NULL,
                                           repo = NULL)
{
  type <- type %||% record$Type
  name <- name %||% renv_restore_retrieve_cran_archive_name(record, type)
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
  path <- case(
    type == "binary" ~ renv_paths_binary(record$Package, name),
    type == "source" ~ renv_paths_source(record$Package, name)
  )

  renv_restore_retrieve_package(record, url, path, type)

}


renv_restore_retrieve_package <- function(record, url, path, type) {

  # if we have a cache entry, we can skip retrieval and just use that later
  cache <- renv_cache_package_path(record)
  if (file.exists(cache))
    path <- cache

  # download the package
  if (!renv_file_exists(path) && !identical(path, cache)) {
    ensure_parent_directory(path)
    status <- catch(download(url, destfile = path))
    if (inherits(status, "error") || identical(status, FALSE))
      return(status)
  }

  # ensure its dependencies are retrieved as well
  deps <- renv_dependencies_discover_description(path)
  for (package in deps$Package)
    renv_restore_retrieve(package)

  # augment record with path, type information
  record$Path <- path
  record$Type <- type

  # store the record for later use
  state <- renv_restore_state()
  state$records$push(record)

  # store requirement information
  if (!empty(deps)) {
    requirements <- state$requirements
    for (i in seq_len(nrow(deps))) {
      row <- deps[i, ]
      package <- row$Package
      if (is.null(requirements[[package]]))
        requirements[[package]] <- stack()
      requirements[[package]]$push(row)
    }
  }

  # return TRUE to indicate successful retrieval
  return(TRUE)

}

renv_restore_retrieve_unknown_source <- function(record) {
  fmt <- "can't retrieve package '%s': '%s' is an unrecognized source"
  stopf(fmt, record$Package, record$Source, call. = FALSE)
}
