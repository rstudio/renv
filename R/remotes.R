
#' Resolve a Remote
#'
#' Given a remote specification, resolve it into an `renv` package record that
#' can be used for download and installation (e.g. with [install]).
#'
#' @param spec A remote specification.
#'
remote <- function(spec) {
  renv_scope_error_handler()
  renv_remotes_resolve(spec)
}

# take a short-form remotes entry, and generate a package record
renv_remotes_resolve <- function(entry) {

  # check for already-resolved lists
  if (is.list(entry))
    return(entry)

  # check for URLs
  if (grepl("^(?:file|https?)://", entry))
    return(renv_remotes_resolve_url(entry, quiet = TRUE))

  # check for paths to existing local files
  local <-
    fileext(entry) %in% c(".tar.gz", ".tgz", ".zip") ||
    renv_path_absolute(entry)

  if (local) {
    record <- catch(renv_remotes_resolve_local(entry))
    if (!inherits(record, "error"))
      return(record)
  }

  # handle errors (add a bit of extra context)
  error <- function(e) {
    fmt <- "failed to parse remote '%s'"
    prefix <- sprintf(fmt, entry)
    message <- paste(prefix, e$message, sep = " -- ")
    stop(simpleError(message = message, call = e$call))
  }

  # attempt the parse
  withCallingHandlers(
    renv_remotes_resolve_impl(entry),
    error = error
  )

}

renv_remotes_resolve_impl <- function(entry) {

  parsed <- renv_remotes_parse(entry)

  resolved <- switch(
    parsed$type,
    bitbucket  = renv_remotes_resolve_bitbucket(parsed),
    gitlab     = renv_remotes_resolve_gitlab(parsed),
    github     = renv_remotes_resolve_github(parsed),
    repository = renv_remotes_resolve_repository(parsed),
    stopf("unknown remote type '%s'", parsed$type %||% "<NA>")
  )

  reject(resolved, is.null)

}

renv_remotes_parse_impl <- function(entry, pattern, fields) {

  matches <- regexec(pattern, entry)
  strings <- regmatches(entry, matches)[[1]]
  if (empty(strings))
    stopf("'%s' is not a valid remote", entry)

  if (length(fields) != length(strings))
    stop("internal error: field length mismatch in renv_remotes_parse_impl")

  names(strings) <- fields
  remote <- as.list(strings)
  lapply(remote, function(item) if (nzchar(item)) item)

}

renv_remotes_parse_repos <- function(entry) {

  pattern <- paste0(
    "^",                               # start
    "(?:([^:]+)::)?",                  # optional repository name
    "([[:alnum:].]+)",                 # package name
    "(?:@([[:digit:]_.-]+))?",         # optional package version
    "$"
  )

  fields <- c("entry", "repository", "package", "version")
  renv_remotes_parse_impl(entry, pattern, fields)

}

renv_remotes_parse_remote <- function(entry) {

  pattern <- paste0(
    "^",
    "(?:([^@:]+)(?:@([^:]+))?::)?",    # optional prefix, providing type + host
    "([^/#@:]+)",                      # a username
    "(?:/([^@#:]+))?",                 # a repository (allow sub-repositories)
    "(?::([^@#:]+))?",                 # optional subdirectory
    "(?:#([^@#:]+))?",                 # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?",                 # optional ref (e.g. branch or commit)
    "$"
  )

  fields <- c("entry", "type", "host", "user", "repo", "subdir", "pull", "ref")
  parsed <- renv_remotes_parse_impl(entry, pattern, fields)

  if (!nzchar(parsed$repo))
    stopf("'%s' is not a valid remote", entry)

  parsed

}

renv_remotes_parse <- function(entry) {

  parsed <- catch(renv_remotes_parse_repos(entry))
  if (!inherits(parsed, "error")) {
    parsed$type <- "repository"
    return(parsed)
  }

  parsed <- catch(renv_remotes_parse_remote(entry))
  if (!inherits(parsed, "error")) {
    parsed$type <- parsed$type %||% "github"
    return(parsed)
  }

  stopf("failed to parse remote spec '%s'", entry)

}

renv_remotes_resolve_bitbucket <- function(entry) {

  user   <- entry$user
  repo   <- entry$repo
  subdir <- entry$subdir
  ref    <- entry$ref %||% "master"

  host <- entry$host %||% config$bitbucket.host()

  # get commit sha for ref
  fmt <- "%s/repositories/%s/%s/commit/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref)

  destfile <- renv_tempfile("renv-bitbucket-")
  download(url, destfile = destfile, type = "bitbucket", quiet = TRUE)
  json <- renv_json_read(file = destfile)
  sha <- json$hash

  # get DESCRIPTION file
  fmt <- "%s/repositories/%s/%s/src/%s/DESCRIPTION"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref)

  destfile <- renv_tempfile("renv-description-")
  download(url, destfile = destfile, type = "bitbucket", quiet = TRUE)
  desc <- renv_dcf_read(destfile)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "Bitbucket",
    RemoteType     = "bitbucket",
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha
  )

}

renv_remotes_resolve_repository <- function(entry) {

  package <- entry$package
  if (package %in% rownames(renv_installed_packages_base()))
    return(renv_remotes_resolve_base(package))

  version <- entry$version
  repository <- entry$repository

  list(
    Package    = package,
    Version    = version,
    Source     = "Repository",
    Repository = repository
  )

}

renv_remotes_resolve_base <- function(package) {

  list(
    Package = package,
    Version = renv_package_version(package),
    Source  = "R"
  )

}

renv_remotes_resolve_github_sha_pull <- function(host, user, repo, pull) {

  fmt <- "%s/repos/%s/%s/pulls/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, pull)
  jsonfile <- renv_tempfile("renv-json-")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)
  json$head$sha

}

renv_remotes_resolve_github_sha_ref <- function(host, user, repo, ref) {

  fmt <- "%s/repos/%s/%s/commits/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref %||% "master")
  headers <- c(Accept = "application/vnd.github.v2.sha")
  shafile <- renv_tempfile("renv-sha-")
  download(url, destfile = shafile, type = "github", quiet = TRUE, headers = headers)
  sha <- renv_file_read(shafile)

  # check for JSON response (in case our headers weren't sent)
  if (nchar(sha) > 40) {
    json <- renv_json_read(text = sha)
    sha <- json$sha
  }

  sha

}

renv_remotes_resolve_github_description <- function(host, user, repo, subdir, sha) {

  # form DESCRIPTION path
  subdir <- subdir %||% ""
  parts <- c(if (nzchar(subdir)) subdir, "DESCRIPTION")

  descpath <- paste(parts, collapse = "/")

  # get the DESCRIPTION contents
  fmt <- "%s/repos/%s/%s/contents/%s?ref=%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, descpath, sha)
  jsonfile <- renv_tempfile("renv-json-")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)
  contents <- renv_base64_decode(json$content)

  # normalize newlines
  contents <- gsub("\r\n", "\n", contents, fixed = TRUE)

  # write to file and read back in
  descfile <- renv_tempfile("renv-description-")
  writeLines(contents, con = descfile)
  renv_dcf_read(descfile)

}

renv_remotes_resolve_github <- function(entry) {

  user   <- entry$user
  repo   <- entry$repo
  subdir <- entry$subdir
  pull   <- entry$pull %||% ""
  ref    <- entry$ref %||% "master"

  host <- entry$host %||% config$github.host()

  # resolve the sha associated with the ref / pull
  sha <- case(
    nzchar(pull) ~ renv_remotes_resolve_github_sha_pull(host, user, repo, pull),
    nzchar(ref)  ~ renv_remotes_resolve_github_sha_ref(host, user, repo, ref)
  )

  desc <- renv_remotes_resolve_github_description(host, user, repo, subdir, sha)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "GitHub",
    RemoteType     = "github",
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha
  )

}

renv_remotes_resolve_gitlab <- function(entry) {

  user   <- entry$user
  repo   <- entry$repo
  subdir <- entry$subdir %||% ""
  ref    <- entry$ref %||% "master"

  parts <- c(if (nzchar(subdir)) subdir, "DESCRIPTION")
  descpath <- URLencode(paste(parts, collapse = "/"), reserved = TRUE)

  host <- entry$host %||% config$gitlab.host()

  # retrieve sha associated with this ref
  fmt <- "%s/api/v4/projects/%s/repository/commits/%s"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf(fmt, origin, id, ref)

  destfile <- renv_tempfile("renv-gitlab-commits-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  json <- renv_json_read(file = destfile)
  sha <- json$id

  # retrieve DESCRIPTION file
  fmt <- "%s/api/v4/projects/%s/repository/files/%s/raw?ref=%s"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf(fmt, origin, id, descpath, ref)

  destfile <- renv_tempfile("renv-description-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  desc <- renv_dcf_read(destfile)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "GitLab",
    RemoteType     = "gitlab",
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha
  )

}

renv_remotes_resolve_url <- function(entry, quiet = FALSE) {

  tempfile <- renv_tempfile("renv-url-")
  writeLines(entry, con = tempfile)
  hash <- tools::md5sum(tempfile)

  ext <- fileext(entry, default = ".tar.gz")
  name <- paste(hash, ext, sep = "")
  path <- renv_paths_source("url", name)

  ensure_parent_directory(path)
  download(entry, path, quiet = quiet)

  desc <- renv_description_read(path)

  list(
    Package    = desc$Package,
    Version    = desc$Version,
    Source     = "URL",
    RemoteType = "url",
    RemoteUrl  = entry,
    Path       = path
  )

}

renv_remotes_resolve_local <- function(entry) {

  # check for existing path
  path <- renv_path_normalize(entry, winslash = "/", mustWork = TRUE)

  # first, check for a common extension
  if (renv_archive_type(entry) %in% c("tar", "zip"))
    return(renv_remotes_resolve_local_impl(path))

  # otherwise, if this is the path to a package project, use the sources as-is
  if (renv_project_type(path) == "package")
    return(renv_remotes_resolve_local_impl(path))

  stopf("there is no package at path '%s'", entry)

}

renv_remotes_resolve_local_impl <- function(path) {

  desc <- renv_description_read(path)
  list(
    Package   = desc$Package,
    Version   = desc$Version,
    Source    = path,
    Cacheable = FALSE
  )

}
