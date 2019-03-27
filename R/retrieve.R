
# this routine retrieves a package + its dependencies, and as a side
# effect populates the restore state's `retrieved` member with a
# list of package records which can later be used for install
renv_retrieve <- function(packages, records = NULL) {

  # TODO: parallel?
  for (package in packages)
    renv_retrieve_impl(package, records)

  state <- renv_restore_state()
  return(state$retrieved$data())

}

renv_retrieve_impl <- function(package, records) {

  # skip 'R' package that might be passed in here
  if (package == "R")
    return()

  # skip 'base' packages
  base <- renv_installed_packages_base()
  if (identical(base[package, "Priority"], "base"))
    return()

  # if we've already attempted retrieval of this package, skip
  state <- renv_restore_state()
  if (visited(package, envir = state$retrieved.env))
    return()

  # extract record for package
  records <- records %||% state$records
  record <- records[[package]] %||% renv_restore_missing_record(package)

  # if the requested record already exists in the cache, we can finish early
  path <- renv_cache_package_path(record)
  if (file.exists(path))
    return(renv_retrieve_successful(record, path))

  # otherwise, try and restore from external source
  source <- tolower(record[["Source"]] %||% "CRAN")
  switch(source,
         cran         = renv_retrieve_cran(record),
         bioconductor = renv_retrieve_bioconductor(record),
         bitbucket    = renv_retrieve_bitbucket(record),
         git          = renv_retrieve_git(record),
         github       = renv_retrieve_github(record),
         renv_retrieve_unknown_source(record)
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

renv_retrieve_bioconductor <- function(record) {

  # activate bioconductor repositories in this context
  repos <- getOption("repos")
  options(repos = unique(c(renv_bioconductor_repos(), repos)))
  on.exit(options(repos = repos), add = TRUE)

  renv_retrieve_cran(record)
}

renv_retrieve_github <- function(record) {

  fmt <- "https://%s/repos/%s/%s/tarball/%s"

  # download the tarball
  url <- with(record, sprintf(fmt, RemoteHost, RemoteUsername, RemoteRepo, RemoteSha))
  suffix <- with(record, sprintf("%s/%s.tar.gz", Package, RemoteSha))
  path <- renv_paths_source(suffix)
  renv_retrieve_package(record, url, path)

}

renv_retrieve_bitbucket <- function(record) {
  call <- deparse(sys.call()[[1]])
  stopf("'%s()' is not yet implemented", call)
}

# TODO: is there a better way to check out a single
# particular commit, without cloning the whole repo?
renv_retrieve_git <- function(record) {

  # clone the repository
  dir <- tempfile("renv-git-")
  args <- c("clone", shQuote(record$RemoteUrl), shQuote(dir))
  system2(git(), args, stdout = TRUE, stderr = TRUE)

  # enter the directory and check out the required commit
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  args <- c("checkout", shQuote(record$RemoteSha))
  system2(git(), args, stdout = TRUE, stderr = TRUE)

  # construct appropriate path for package
  desc <- renv_description_read(dir)
  name <- sprintf("%s_%s.tar.gz", desc$Package, desc$Version)
  path <- renv_paths_source(record$Package, name)

  # use 'file' URL for retrieval
  url  <- paste("file://", dir, sep = "")

  # retrieve the package
  renv_retrieve_package(record, url, path)

}

renv_retrieve_cran <- function(record) {

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
    if (inherits(status, "error"))
      stop(status)

    if (identical(status, TRUE))
      return(TRUE)
  }

  stopf("failed to retrieve package '%s' from CRAN", record$Package)

}

renv_retrieve_cran_archive_name_binary <- function(record) {
  sysname <- Sys.info()[["sysname"]]
  suffix <- switch(sysname, Darwin = "tgz", Windows = "zip", "tar.gz")
  sprintf("%s_%s.%s", record$Package, record$Version, suffix)
}

renv_retrieve_cran_archive_name_source <- function(record) {
  sprintf("%s_%s.tar.gz", record$Package, record$Version)
}

renv_retrieve_cran_archive_name <- function(record, type) {
  case(
    type == "binary" ~ renv_retrieve_cran_archive_name_binary(record),
    type == "source" ~ renv_retrieve_cran_archive_name_source(record)
  )
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
  path <- case(
    type == "binary" ~ renv_paths_binary(record$Package, name),
    type == "source" ~ renv_paths_source(record$Package, name)
  )

  renv_retrieve_package(record, url, path)

}


renv_retrieve_package <- function(record, url, path) {

  # download the package
  if (!renv_file_exists(path)) {
    ensure_parent_directory(path)
    status <- catch(download(url, destfile = path))
    if (inherits(status, "error") || identical(status, FALSE))
      return(status)
  }

  renv_retrieve_successful(record, path)

}

renv_retrieve_successful <- function(record, path) {

  # augment record with information from DESCRIPTION file
  desc <- renv_description_read(path)
  record$Package <- desc$Package
  record$Version <- desc$Version
  record$Path <- path

  # ensure its dependencies are retrieved as well
  state <- renv_restore_state()
  deps <- renv_dependencies_discover_description(path)
  if (state$recursive)
    for (package in unique(deps$Package))
      renv_retrieve(package)

  # record package as retrieved
  state$retrieved$push(record)

  # record this package's requirements
  requirements <- state$requirements

  rowapply(deps, function(dep) {
    package <- dep$Package
    requirements[[package]] <<- requirements[[package]] %||% stack()
    requirements[[package]]$push(dep)
  })

  TRUE

}

renv_retrieve_unknown_source <- function(record) {
  fmt <- "package '%s' was installed from an unknown source: installing latest version from CRAN instead"
  warningf(fmt, record$Package)
  record <- renv_restore_missing_record(record$Package)
  renv_retrieve_cran(record)
}
