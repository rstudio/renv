
# this routine retrieves a package + its dependencies, and as a side
# effect populates the restore state's `retrieved` member with a
# list of package records which can later be used for install
renv_retrieve <- function(packages, records = NULL) {

  # cache set of installed packages (avoid re-querying on each retrieval)
  renv_global_set("installed.packages", renv_installed_packages())
  on.exit(renv_global_clear("installed.packages"), add = TRUE)

  # TODO: parallel?
  for (package in packages)
    renv_retrieve_impl(package, records)

  state <- renv_restore_state()
  data <- state$retrieved$data()
  names(data) <- extract_chr(data, "Package")
  data

}

renv_retrieve_impl <- function(package, records = NULL) {

  # skip packages with 'base' priority
  if (renv_package_priority(package) == "base")
    return()

  # if we've already attempted retrieval of this package, skip
  state <- renv_restore_state()
  if (visited(package, envir = state$retrieved.env))
    return()

  # extract record for package
  records <- records %||% state$records
  record <- records[[package]] %||% renv_retrieve_missing_record(package)

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
         renv_retrieve_unknown_source(record)
  )

}

renv_retrieve_path <- function(record) {

  package <- record$Package
  source <- record$Source %||% record$RemoteType %||% ""

  fields <- c("Package", "Version", "RemoteSha")
  matches <- record[intersect(names(record), fields)]
  name <- sprintf("%s.tar.gz", paste(matches, collapse = "_"))

  renv_paths_source(package, source, name)

}

renv_retrieve_bioconductor <- function(record) {

  # activate bioconductor repositories in this context
  repos <- getOption("repos")
  options(repos = unique(c(renv_bioconductor_repos(), repos)))
  on.exit(options(repos = repos), add = TRUE)

  renv_retrieve_cran(record)
}

renv_retrieve_github <- function(record) {

  record$RemoteHost <- record$RemoteHost %||% "api.github.com"

  fmt <- "https://%s/repos/%s/%s/tarball/%s"
  url <- with(record, sprintf(fmt, RemoteHost, RemoteUsername, RemoteRepo, RemoteSha))
  path <- renv_retrieve_path(record)
  renv_retrieve_package(record, url, path, overwrite = TRUE)

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

  renv_retrieve_package(record, url, path, overwrite = TRUE)

}

renv_retrieve_bitbucket <- function(record) {

  remotes <- renv_retrieve_require("remotes", "Bitbucket")

  remote <- remotes$bitbucket_remote(
    repo      = file.path(record$RemoteUsername, record$RemoteRepo),
    ref       = record$RemoteSha %||% record$RemoteRef,
    sha       = record$RemoteSha,
    host      = record$RemoteHost %||% "api.bitbucket.org/2.0",
    subdir    = record$RemoteSubdir,
    auth_user = Sys.getenv("BITBUCKET_USER", unset = NA) %NA% NULL,
    password  = Sys.getenv("BITBUCKET_PASSWORD", unset = NA) %NA% NULL
  )

  renv_retrieve_remote(record, remote, remotes)

}

renv_retrieve_git <- function(record) {

  remotes <- renv_retrieve_require("remotes", "Git")

  remote <- remotes$git_remote(
    url = record$RemoteUrl,
    ref = record$RemoteSha %||% record$RemoteRef
  )

  renv_retrieve_remote(record, remote, remotes)

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


renv_retrieve_package <- function(record, url, path, overwrite = FALSE) {

  # download the package
  if (overwrite || !renv_file_exists(path)) {
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

  # add in path information to record (used during install)
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
  record <- renv_retrieve_missing_record(record$Package)
  renv_retrieve_cran(record)
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
  fields <- c(record$Package, record$Version, record$RemoteSha)
  name <- sprintf("%s.tar.gz", paste(fields, collapse = "_"))
  path <- renv_paths_source(record$Package, record$Source, name)
  ensure_parent_directory(path)

  renv_retrieve_package(record, url, path, overwrite = TRUE)

}
