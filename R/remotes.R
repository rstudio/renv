
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

  # check for URLs
  if (grepl("^(?:file|https?)://", entry))
    return(renv_remotes_resolve_url(entry))

  # check for paths to existing local files
  if (path_absolute(entry) && file.exists(entry)) {
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
  switch(
    parsed$type,
    bitbucket  = renv_remotes_resolve_bitbucket(parsed),
    gitlab     = renv_remotes_resolve_gitlab(parsed),
    github     = renv_remotes_resolve_github(parsed),
    repository = renv_remotes_resolve_repository(parsed),
    stopf("unknown remote type '%s'", parsed$type %||% "<NA>")
  )

}

renv_remotes_parse <- function(entry) {

  pattern <- paste0(
    "(?:([^@:]+)(?:@([^:]+))?::)?",    # optional prefix, providing type + host
    "([^/#@:]+)",                      # a username
    "(?:/([^@#:]+))?",                 # a repository (allow sub-repositories)
    "(?::([^@#:]+))?",                 # optional subdirectory
    "(?:#([^@#:]+))?",                 # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?"                  # optional ref (e.g. branch or commit)
  )

  matches <- regexec(pattern, entry)
  strings <- regmatches(entry, matches)[[1]]
  if (empty(strings))
    stopf("'%s' is not a valid remote", entry)

  parsed <- list(
    entry  = strings[[1]],
    type   = strings[[2]],
    host   = strings[[3]],
    user   = strings[[4]],
    repo   = strings[[5]],
    subdir = strings[[6]],
    pull   = strings[[7]],
    ref    = strings[[8]]
  )

  # handle short-forms
  if (!nzchar(parsed$type)) {
    type <- if (nzchar(parsed$repo)) "github" else "repository"
    parsed$type <- type
  }

  parsed

}

renv_remotes_resolve_bitbucket <- function(entry) {

  user   <- entry$user
  repo   <- entry$repo
  subdir <- entry$subdir
  ref    <- entry$ref %""% "master"

  host <-
    entry$host %""%
    renv_config("bitbucket.host", default = "api.bitbucket.org/2.0")

  fmt <- "https://%s/repositories/%s/%s/src/%s/DESCRIPTION"
  url <- sprintf(fmt, host, user, repo, ref)

  destfile <- renv_tempfile("renv-description-")
  download(url, destfile = destfile, type = "bitbucket", quiet = TRUE)
  desc <- renv_dcf_read(destfile)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "Bitbucket",
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref
  )

}

renv_remotes_resolve_repository <- function(entry) {

  package <- entry$user
  if (package %in% rownames(renv_installed_packages_base()))
    return(renv_remotes_resolve_base(package))

  version <- entry$ref %""% "latest"
  if (identical(version, "latest"))
    return(renv_records_repos_latest(package))

  list(
    Package    = package,
    Version    = version,
    Source     = "Repository"
  )

}

renv_remotes_resolve_base <- function(package) {

  list(
    Package = package,
    Version = renv_package_version(package),
    Source  = "Base"
  )

}

renv_remotes_resolve_github_sha_pull <- function(host, user, repo, pull) {

  fmt <- "https://%s/repos/%s/%s/pulls/%s"
  url <- sprintf(fmt, host, user, repo, pull)
  jsonfile <- renv_tempfile("renv-json-")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)
  json$head$sha

}

renv_remotes_resolve_github_sha_ref <- function(host, user, repo, ref) {

  fmt <- "https://%s/repos/%s/%s/commits/%s"
  url <- sprintf(fmt, host, user, repo, ref)
  headers <- c(Accept = "application/vnd.github.v2.sha")
  shafile <- renv_tempfile("renv-sha-")
  download(url, destfile = shafile, type = "github", quiet = TRUE, headers = headers)
  sha <- readChar(shafile, file.info(shafile)$size, TRUE)

  # check for JSON response (in case our headers weren't sent)
  if (nchar(sha) > 40) {
    json <- renv_json_read(text = sha)
    sha <- json$sha
  }

  sha

}

renv_remotes_resolve_github_description <- function(host, user, repo, subdir, sha) {

  # form DESCRIPTION path
  parts <- c(if (nzchar(subdir)) subdir, "DESCRIPTION")
  descpath <- paste(parts, collapse = "/")

  # get the DESCRIPTION contents
  fmt <- "https://%s/repos/%s/%s/contents/%s?ref=%s"
  url <- sprintf(fmt, host, user, repo, descpath, sha)
  jsonfile <- renv_tempfile("renv-json-")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)
  contents <- renv_base64_decode(json$content)

  # write to file and read back in
  descfile <- renv_tempfile("renv-description-")
  writeLines(contents, con = descfile)
  renv_dcf_read(descfile)

}

renv_remotes_resolve_github <- function(entry) {

  user   <- entry$user
  repo   <- entry$repo
  subdir <- entry$subdir
  pull   <- entry$pull
  ref    <- entry$ref %""% "master"

  host <-
    entry$host %""%
    renv_config("github.host", default = "api.github.com")

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
  subdir <- entry$subdir
  ref    <- entry$ref %""% "master"

  parts <- c(if (nzchar(subdir)) subdir, "DESCRIPTION")
  descpath <- URLencode(paste(parts, collapse = "/"), reserved = TRUE)

  host <-
    entry$host %""%
    renv_config("gitlab.host", default = "gitlab.com")

  fmt <- "https://%s/api/v4/projects/%s/repository/files/%s/raw?ref=%s"
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf(fmt, host, id, descpath, ref)

  destfile <- renv_tempfile("renv-description-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  desc <- renv_dcf_read(destfile)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "GitLab",
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref
  )

}

renv_remotes_resolve_url <- function(entry) {

  tempfile <- renv_tempfile("renv-url-")
  writeLines(entry, con = tempfile)
  hash <- tools::md5sum(tempfile)

  ext <- fileext(entry, default = ".tar.gz")
  name <- paste(hash, ext, sep = "")
  path <- renv_paths_source("url", name)

  ensure_parent_directory(path)
  download(entry, path, quiet = TRUE)

  desc <- renv_description_read(path)

  list(
    Package   = desc$Package,
    Version   = desc$Version,
    Source    = "URL",
    Path      = path,
    RemoteUrl = entry
  )

}

renv_remotes_resolve_local <- function(entry) {

  # check for existing path
  path <- normalizePath(entry, winslash = "/", mustWork = TRUE)

  # first, check for a common extension
  if (renv_archive_type(entry) == "tar")
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
