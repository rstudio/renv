
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
renv_remotes_resolve <- function(entry, latest = FALSE) {

  # check for already-resolved lists
  if (is.null(entry) || is.list(entry))
    return(entry)

  # check for URLs
  if (grepl("^(?:file|https?)://", entry))
    return(renv_remotes_resolve_url(entry, quiet = TRUE))

  # check for paths to existing local files
  first <- substring(entry, 1L, 1L)
  local <- first %in% c("~", "/", ".") || renv_path_absolute(entry)

  if (local) {
    record <- catch(renv_remotes_resolve_path(entry))
    if (!inherits(record, "error"))
      return(record)
  }

  # define error handler (tag error with extra context when possible)
  error <- function(e) {

    # build error message
    fmt <- "failed to resolve remote '%s'"
    prefix <- sprintf(fmt, entry)
    message <- paste(prefix, e$message, sep = " -- ")

    # if the error occurred while running tests, and the error appears
    # to be due to an HTTP error (unauthorized), then just skip instead
    if (renv_tests_running()) {
      unauth <- any(grepl("403", e$message %||% ""))
      if (unauth)
        testthat::skip("Ignoring transient 403 error")
    }

    # otherwise, propagate the error
    stop(simpleError(message = message, call = e$call))

  }

  # attempt the parse
  withCallingHandlers(
    renv_remotes_resolve_impl(entry, latest),
    error = error
  )

}

renv_remotes_resolve_impl <- function(entry, latest = FALSE) {

  parsed <- renv_remotes_parse(entry)

  resolved <- switch(
    parsed$type,
    bitbucket  = renv_remotes_resolve_bitbucket(parsed),
    gitlab     = renv_remotes_resolve_gitlab(parsed),
    github     = renv_remotes_resolve_github(parsed),
    repository = renv_remotes_resolve_repository(parsed, latest),
    git        = renv_remotes_resolve_git(parsed),
    stopf("unknown remote type '%s'", parsed$type %||% "<NA>")
  )

  reject(resolved, is.null)

}

renv_remotes_parse_impl <- function(entry, pattern, fields, perl = FALSE) {

  matches <- regexec(pattern, entry, perl = perl)
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

  renv_remotes_parse_finalize(parsed)

}

renv_remotes_parse_git <- function(entry) {
  pattern <- paste0(
    "^",
    "(?:([^@:]+)::)?",  # optional prefix, providing type
    "(", # URL start
      "(?:(https?|git|ssh)://)?",   # protocol
      "(?:([^@]+)@)?",    # login (probably git)
      # stack overflow: https://stackoverflow.com/a/26987741/9238801
      "(",  # host start
        "(?:(?:(?!-))(?:xn--|_{1,1})?[a-z0-9-]{0,61}[a-z0-9]{1,1}\\.)*",
        "(?:xn--)?",
        "(?:[a-z0-9][a-z0-9\\-]{0,60}|[a-z0-9-]{1,30}\\.[a-z]{2,})",
      ")",  # host end
      "[/:]([-\\.a-zA-Z]+)",             # a username
      "(?:/([^@#:]+?))?",                # a repository (allow sub-repositories)
      "(?:\\.(git))?",                     # optional .git extension
    ")", # URL end
    "(?::([^@#:]+))?",                 # optional subdirectory
    "(?:#([^@#:]+))?",                 # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?",                 # optional ref (e.g. branch or commit)
    "$"
  )

  fields <- c(
    "entry",
      "type",
      "url",
        "protocol", "login", "host",
        "user", "repo", "ext",
      "subdir",
      "pull",
      "ref"
    )
  parsed <- renv_remotes_parse_impl(entry, pattern, fields, perl = TRUE)

  if (!nzchar(parsed$repo))
    stopf("'%s' is not a valid remote", entry)

  # If type has not been found & repo looks like a git repo, set it as git
  # (note that this parser also accepts entries which are not truly git
  # references, so we try to "fix up" after the fact)
  if ("git" %in% c(parsed$type, parsed$ext, parsed$protocol))
    parsed$type <- tolower(parsed$type %||% "git")

  renv_remotes_parse_finalize(parsed)
}

renv_remotes_parse_finalize <- function(parsed) {
  # default remote type is github
  parsed$type <- tolower(parsed$type %||% "github")

  # custom finalization for different remote types
  case(
    parsed$type == "github" ~ renv_remotes_parse_finalize_github(parsed),
    TRUE                    ~ parsed
  )

}

renv_remotes_parse_finalize_github <- function(parsed) {

  # split repo spec into pieces
  repo <- parsed$repo %||% ""
  parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  if (length(parts) < 2)
    return(parsed)

  # form subdir from tail of repo
  parsed$repo   <- paste(head(parts, n = 1L),  collapse = "/")
  parsed$subdir <- paste(tail(parts, n = -1L), collapse = "/")

  # return modified remote
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

  parsed <- catch(renv_remotes_parse_git(entry))
  if (!inherits(parsed, "error")) {
    parsed$type <- parsed$type %||% "git"
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

  destfile <- renv_scope_tempfile("renv-bitbucket-")
  download(url, destfile = destfile, type = "bitbucket", quiet = TRUE)
  json <- renv_json_read(file = destfile)
  sha <- json$hash

  # get DESCRIPTION file
  fmt <- "%s/repositories/%s/%s/src/%s/DESCRIPTION"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref)

  destfile <- renv_scope_tempfile("renv-description-")
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

renv_remotes_resolve_repository <- function(entry, latest) {

  package <- entry$package
  if (package %in% renv_packages_base())
    return(renv_remotes_resolve_base(package))

  version <- entry$version
  repository <- entry$repository

  if (latest && is.null(version)) {
    entry <- renv_available_packages_latest(package)
    version <- entry$Version
  }

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
  jsonfile <- renv_scope_tempfile("renv-json-")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)
  json$head$sha

}

renv_remotes_resolve_github_sha_ref <- function(host, user, repo, ref) {

  # build url for github endpoint
  fmt <- "%s/repos/%s/%s/commits/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref %||% "master")

  # prepare headers
  headers <- c(Accept = "application/vnd.github.v2.sha")

  # make request to endpoint
  shafile <- renv_scope_tempfile("renv-sha-")
  download(
    url,
    destfile = shafile,
    type = "github",
    quiet = TRUE,
    headers = headers
  )

  # read downloaded content
  sha <- renv_file_read(shafile)

  # check for JSON response (in case our headers weren't sent)
  if (nchar(sha) > 40L) {
    json <- renv_json_read(text = sha)
    sha <- json$sha
  }

  sha

}

renv_remotes_resolve_github_description <- function(host, user, repo, subdir, sha) {

  # form DESCRIPTION path
  subdir <- subdir %||% ""
  parts <- c(
    if (nzchar(subdir)) URLencode(subdir),
    "DESCRIPTION"
  )

  descpath <- paste(parts, collapse = "/")

  # get the DESCRIPTION contents
  fmt <- "%s/repos/%s/%s/contents/%s?ref=%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, descpath, sha)
  jsonfile <- renv_scope_tempfile("renv-json-")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)
  contents <- renv_base64_decode(json$content)

  # normalize newlines
  contents <- gsub("\r\n", "\n", contents, fixed = TRUE)

  # write to file and read back in
  descfile <- renv_scope_tempfile("renv-description-")
  writeLines(contents, con = descfile)
  renv_dcf_read(descfile)

}

renv_remotes_resolve_github_ref <- function(host, user, repo) {

  tryCatch(
    renv_remotes_resolve_github_ref_impl(host, user, repo),
    error = function(e) {
      warning(e)
      getOption("renv.github.default_branch", default = "master")
    }
  )

}

renv_remotes_resolve_github_ref_impl <- function(host, user, repo) {

  # build url to repos endpoint
  fmt <- "%s/repos/%s/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo)

  # download JSON data at endpoint
  jsonfile <- renv_scope_tempfile("renv-github-ref-", fileext = ".json")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)

  # read default branch
  json$default_branch %||% "master"

}

renv_remotes_resolve_github <- function(entry) {

  # resolve the reference associated with this repository
  host <- entry$host %||% config$github.host()
  user <- entry$user
  repo <- entry$repo
  ref  <- entry$ref %||% renv_remotes_resolve_github_ref(host, user, repo)

  # resolve the sha associated with the ref / pull
  pull   <- entry$pull %||% ""
  sha <- case(
    nzchar(pull) ~ renv_remotes_resolve_github_sha_pull(host, user, repo, pull),
    nzchar(ref)  ~ renv_remotes_resolve_github_sha_ref(host, user, repo, ref)
  )

  # read DESCRIPTION
  subdir <- entry$subdir
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

renv_remotes_resolve_git <- function(parsed) {

  package <- parsed$repo
  url <- parsed$url
  subdir <- parsed$subdir

  # handle git ref
  pull <- parsed$pull %||% ""
  ref  <- parsed$ref %||% ""

  # resolve ref from pull if set
  if (nzchar(pull))
    ref <- renv_remotes_resolve_git_pull(ref)

  record <- list(
    Package        = package,
    Version        = "<unknown>",
    Source         = "git",
    RemoteType     = "git",
    RemoteUrl      = url,
    RemoteSubdir   = subdir,
    RemoteRef      = ref
  )

  desc <- renv_remotes_resolve_git_description(record)

  record$Package <- desc$Package
  record$Version <- desc$Version

  record
}


renv_remotes_resolve_git_sha_ref <- function(record) {

  renv_git_preflight()

  data <- list(
    ORIGIN = record$RemoteUrl,
    REF    = record$RemoteRef %||% record$RemoteSha
  )

  args <- c("ls-remote", data$ORIGIN, data$REF)

  # output looks like:
  # SHA1 (tab) refs/path/to/ref_name

  output <- local({
    renv_scope_auth(record)
    renv_scope_git_auth()
    renv_system_exec("git", args, "checking git remote")
  })

  if (empty(output))
    return("")

  # split each line
  # gets a list of character vectors,
  # list(
  #   c("sha1", "refname1"),
  #   c("sha2", "refname2)
  #  )
  # get first sha of first line
  sha <- strsplit(output, "\t", fixed = TRUE)[[1]][1]
  return(sha)
}


renv_remotes_resolve_git_description <- function(record) {
  path <- tempfile("renv-git-")
  ensure_directory(path)

  renv_retrieve_git_impl(record, path)

  subdir <- record$RemoteSubdir
  # subdir may be NULL
  desc <- renv_description_read(path, subdir = subdir)

  desc
}

renv_remotes_resolve_git_pull <- function(pr) {
  # to be able to checkout PR 760:
  # git fetch origin pull/760/head:pr-760
  # or:
  # git fetch origin pull/760/head:pull/760

  # so format for ref is:
  # pull/{ref_number}/head:pr-{ref_number}
  fmt <- "pull/%s/head:pull/%s"

  remote_ref <- sprintf(fmt, pr, pr)
  remote_ref
}

renv_remotes_resolve_gitlab_ref <- function(host, user, repo) {

  tryCatch(
    renv_remotes_resolve_gitlab_ref_impl(host, user, repo),
    error = function(e) {
      warning(e)
      "master"
    }
  )

}

renv_remotes_resolve_gitlab_ref_impl <- function(host, user, repo) {

  # get list of available branches
  fmt <- "%s/api/v4/projects/%s/repository/branches"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf(fmt, origin, id)

  destfile <- renv_scope_tempfile("renv-gitlab-commits-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  json <- renv_json_read(file = destfile)

  # iterate through and find the default
  for (info in json)
    if (identical(info$default, TRUE))
      return(info$name)

  # if no default was found, use master branch
  # (for backwards compatibility with existing projects)
  getOption("renv.gitlab.default_branch", default = "master")

}

renv_remotes_resolve_gitlab <- function(entry) {

  host   <- entry$host %||% config$gitlab.host()
  user   <- entry$user
  repo   <- entry$repo
  subdir <- entry$subdir %||% ""

  ref <- entry$ref %||% renv_remotes_resolve_gitlab_ref(host, user, repo)

  parts <- c(if (nzchar(subdir)) subdir, "DESCRIPTION")
  descpath <- URLencode(paste(parts, collapse = "/"), reserved = TRUE)

  # retrieve sha associated with this ref
  fmt <- "%s/api/v4/projects/%s/repository/commits/%s"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  ref <- URLencode(ref, reserved = TRUE)
  url <- sprintf(fmt, origin, id, ref)

  destfile <- renv_scope_tempfile("renv-gitlab-commits-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  json <- renv_json_read(file = destfile)
  sha <- json$id

  # retrieve DESCRIPTION file
  fmt <- "%s/api/v4/projects/%s/repository/files/%s/raw?ref=%s"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf(fmt, origin, id, descpath, ref)

  destfile <- renv_scope_tempfile("renv-description-")
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

  tempfile <- renv_scope_tempfile("renv-url-")
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

renv_remotes_resolve_path <- function(entry) {

  # check for existing path
  path <- renv_path_normalize(entry, winslash = "/", mustWork = TRUE)

  # first, check for a common extension
  if (renv_archive_type(entry) %in% c("tar", "zip"))
    return(renv_remotes_resolve_path_impl(path))

  # otherwise, if this is the path to a package project, use the sources as-is
  if (renv_project_type(path) == "package")
    return(renv_remotes_resolve_path_impl(path))

  stopf("there is no package at path '%s'", entry)

}

renv_remotes_resolve_path_impl <- function(path) {

  desc <- renv_description_read(path)
  list(
    Package    = desc$Package,
    Version    = desc$Version,
    Source     = "Local",
    RemoteType = "local",
    RemoteUrl  = path,
    Cacheable  = FALSE
  )

}
