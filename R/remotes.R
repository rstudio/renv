
#' Resolve a Remote
#'
#' Given a remote specification, resolve it into an renv package record that
#' can be used for download and installation (e.g. with [install]).
#'
#' @param spec A remote specification. This should be a string, conforming
#'   to the Remotes specification as defined in
#'   <https://remotes.r-lib.org/articles/dependencies.html>.
#'
remote <- function(spec) {
  renv_scope_error_handler()
  renv_remotes_resolve(spec)
}

# take a short-form remotes spec, parse that into a remote,
# and generate a corresponding package record
renv_remotes_resolve <- function(spec, latest = FALSE, infer = FALSE) {

  # check for already-resolved specs
  if (is.null(spec) || is.list(spec))
    return(spec)

  # check for a package name prefix and remove it
  regexps <- .standard_regexps()
  pattern <- sprintf("^%s=", regexps$valid_package_name)
  spec <- sub(pattern, "", spec)

  # remove a trailing slash
  # https://github.com/rstudio/renv/issues/1135
  spec <- gsub("/+$", "", spec, perl = TRUE)

  # check if we should infer the package version
  infer <-
    infer &&
    grepl(renv_regexps_package_name(), spec) &&
    renv_package_installed(spec)

  if (infer)
    spec <- paste(spec, renv_package_version(spec), sep = "@")

  # check for archive URLs -- this is a bit hacky
  if (grepl("^(?:file|https?)://", spec)) {
    for (suffix in c(".zip", ".tar.gz", ".tgz", "/tarball"))
      if (endswith(spec, suffix))
        return(renv_remotes_resolve_url(spec, quiet = TRUE))
  }

  # remove github prefix
  spec <- gsub("^https?://(?:www\\.)?github\\.com/", "", spec)

  # check for paths to existing local files
  first <- substring(spec, 1L, 1L)
  local <- first %in% c("~", "/", ".") || renv_path_absolute(spec)

  if (local) {
    record <- catch(renv_remotes_resolve_path(spec))
    if (!inherits(record, "error"))
      return(record)
  }

  # check for explicit local remotes
  if (grepl("^local::", spec)) {
    spec <- substring(spec, 8L)
    record <- catch(renv_remotes_resolve_path(spec))
    if (!inherits(record, "error"))
      return(record)
  }

  # check for requests to install local packages -- note that depending on how
  # the R package was built / generated, it's possible that it might not adhere
  # to the "typical" R package names, so we try to be a bit flexible here
  ext <- "(?:\\.tar\\.gz|\\.tgz|\\.zip)$"
  if (grepl(ext, spec, perl = TRUE)) {
    pathlike <- tryCatch(file.exists(spec), condition = identity)
    if (identical(pathlike, TRUE)) {
      return(renv_remotes_resolve_path(spec))
    }
  }

  # define error handler (tag error with extra context when possible)
  error <- function(e) {

    # build error message
    fmt <- "failed to resolve remote '%s'"
    prefix <- sprintf(fmt, spec)
    message <- paste(prefix, e$message, sep = " -- ")

    # otherwise, propagate the error
    stop(simpleError(message = message, call = e$call))

  }

  # attempt the parse
  withCallingHandlers(
    renv_remotes_resolve_impl(spec, latest),
    error = error
  )

}

renv_remotes_resolve_impl <- function(spec, latest = FALSE) {

  remote <- renv_remotes_parse(spec)

  # fixup for bioconductor
  isbioc <-
    identical(remote$type, "repository") &&
    identical(remote$repository, "bioc")

  if (isbioc)
    remote$type <- "bioc"

  # treat HEAD refs as an implicit request to use the default branch
  # of the associated remote repository
  # https://github.com/rstudio/renv/issues/2040
  if (identical(remote$ref, "HEAD"))
    remote$ref <- NULL

  resolved <- switch(
    remote$type,
    bioc       = renv_remotes_resolve_bioc(remote),
    bitbucket  = renv_remotes_resolve_bitbucket(remote),
    gitlab     = renv_remotes_resolve_gitlab(remote),
    github     = renv_remotes_resolve_github(remote),
    repository = renv_remotes_resolve_repository(remote, latest),
    git        = renv_remotes_resolve_git(remote),
    url        = renv_remotes_resolve_url(remote$url, quiet = TRUE),
    stopf("unknown remote type '%s'", remote$type %||% "<NA>")
  )

  # ensure that attributes on the record are preserved, but drop NULL entries
  for (key in names(resolved))
    if (is.null(resolved[[key]]))
      resolved[[key]] <- NULL

  resolved

}

renv_remotes_parse_impl <- function(spec, pattern, fields, perl = FALSE) {

  matches <- regexec(pattern, spec, perl = perl)
  strings <- regmatches(spec, matches)[[1]]
  if (empty(strings))
    stopf("'%s' is not a valid remote", spec)

  if (length(fields) != length(strings))
    stop("internal error: field length mismatch in renv_remotes_parse_impl")

  names(strings) <- fields
  remote <- as.list(strings)
  lapply(remote, function(item) if (nzchar(item)) item)

}

renv_remotes_parse_repos <- function(spec) {

  pattern <- paste0(
    "^",                                           # start
    "(?:([^:]+)::)?",                              # optional repository name
    "([[:alnum:].]+)",                             # package name
    "(?:@([[:digit:]_.-]+))?",                     # optional package version
    "$"
  )

  fields <- c("spec", "repository", "package", "version")
  renv_remotes_parse_impl(spec, pattern, fields)

}

renv_remotes_parse_remote <- function(spec) {

  pattern <- paste0(
    "^",
    "(?:([[:alpha:]][[:alnum:].]*[[:alnum:]])=)?",  # optional package name
    "(?:([^@:]+)(?:@([^:]+))?::)?",                 # optional prefix, providing type + host
    "([^/#@:]+)",                                   # a username
    "(?:/([^@#:]+))?",                              # a repository (allow sub-repositories)
    "(?::([^@#:]+))?",                              # optional subdirectory
    "(?:#([^@#:]+))?",                              # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?",                              # optional ref (e.g. branch or commit)
    "$"
  )

  fields <- c(
    "spec", "package", "type",
    "host", "user", "repo",
    "subdir", "pull", "ref"
  )

  remote <- renv_remotes_parse_impl(spec, pattern, fields)
  if (!nzchar(remote$repo))
    stopf("'%s' is not a valid remote", spec)

  renv_remotes_parse_finalize(remote)

}

renv_remotes_parse_gitssh <- function(spec) {

  pattern <- paste0(
    "^",
    "(?:([[:alpha:]][[:alnum:].]*[[:alnum:]])=)?",  # optional package name
    "(?:(git)::)?",                                 # optional git prefix
    "(",                                            # url start
      "([^@]+)@",                                   # user (typically, 'git')
      "([^:]+):",                                   # host
      "([^:#@]+)",                                  # the rest of the repo url
    ")",                                            # url end
    "(?::([^@#:]+))?",                              # optional sub-directory
    "(?:#([^@#:]+))?",                              # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?",                              # optional ref (e.g. branch or commit)
    "$"
  )

  fields <- c(
    "spec", "package", "type", "url",
    "user", "host", "repo",
    "subdir", "pull", "ref"
  )

  remote <- renv_remotes_parse_impl(spec, pattern, fields, perl = TRUE)
  if (!nzchar(remote$repo))
    stopf("'%s' is not a valid remote", spec)

  remote$type <- remote$type %||% "git"
  renv_remotes_parse_finalize(remote)

}

renv_remotes_parse_git <- function(spec) {

  hostpattern <- paste0(
    "(",
      "(?:(?:(?!-))(?:xn--|_{1,1})?[a-z0-9-]{0,61}[a-z0-9]{1,1}\\.)*",
      "(?:xn--)?",
      "(?:[a-z0-9][a-z0-9\\-]{0,60}|[a-z0-9-]{1,30}\\.[a-z]{2,})",
    ")"
  )

  pattern <- paste0(
    "^",
    "(?:([[:alpha:]][[:alnum:].]*[[:alnum:]])=)?",  # optional package name
    "(?:(git)::)?",                                 # optional git prefix
    "(",                                            # URL start
      "(?:(https?|git|ssh)://)?",                   #   protocol
      "(?:([^@]+)@)?",                              #   login (probably git)
      hostpattern,                                  #   host
      "[/:]([\\w_.-]+)",                            #   a username
      "(?:/([^@#:]+?))?",                           #   a repository (allow sub-repositories)
      "(?:\\.(git))?",                              #   optional .git extension
    ")",                                            # URL end
    "(?::([^@#:]+))?",                              # optional sub-directory
    "(?:#([^@#:]+))?",                              # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?",                              # optional ref (e.g. branch or commit)
    "$"
  )

  fields <- c(
    "spec", "package", "type",
    "url", "protocol", "login", "host", "user", "repo", "ext",
    "subdir", "pull", "ref"
  )

  remote <- renv_remotes_parse_impl(spec, pattern, fields, perl = TRUE)
  if (!nzchar(remote$repo))
    stopf("'%s' is not a valid remote", spec)

  # If type has not been found & repo looks like a git repo, set it as git
  # (note that this parser also accepts entries which are not truly git
  # references, so we try to "fix up" after the fact)
  if ("git" %in% c(remote$login, remote$type, remote$ext, remote$protocol))
    remote$type <- tolower(remote$type %||% "git")

  renv_remotes_parse_finalize(remote)

}

# NOTE: to avoid ambiguity with git remote specs, we require URL
# remotes to begin with a 'url::' prefix
renv_remotes_parse_url <- function(spec) {

  pattern <- paste0(
    "^",
    "(?:([[:alpha:]][[:alnum:].]*[[:alnum:]])=)?",  # optional package name
    "(url)::",                                      # type (required for URL remotes)
    "((https?)://([^:]+))",                         # url, protocol, path
    "(?::([^@#:]+))?",                              # optional subdir
    "$"
  )

  fields <- c("spec", "package", "type", "url", "protocol", "path", "subdir")
  remote <- renv_remotes_parse_impl(spec, pattern, fields, perl = TRUE)
  if (!nzchar(remote$url))
    stopf("'%s' is not a valid remote", spec)

  renv_remotes_parse_finalize(remote)
}

renv_remotes_parse_finalize <- function(remote) {

  # default remote type is github
  remote$type <- tolower(remote$type %||% "github")

  # custom finalization for different remote types
  case(
    remote$type == "github" ~ renv_remotes_parse_finalize_github(remote),
    TRUE                    ~ remote
  )

}

renv_remotes_parse_finalize_github <- function(remote) {

  # split repo spec into pieces
  repo <- remote$repo %||% ""
  parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  if (length(parts) < 2)
    return(remote)

  # form subdir from tail of repo
  remote$repo   <- paste(head(parts, n = 1L),  collapse = "/")
  remote$subdir <- paste(tail(parts, n = -1L), collapse = "/")

  # return modified remote
  remote

}

renv_remotes_parse <- function(spec) {

  remote <- catch(renv_remotes_parse_repos(spec))
  if (!inherits(remote, "error")) {
    remote$type <- "repository"
    return(remote)
  }

  remote <- catch(renv_remotes_parse_remote(spec))
  if (!inherits(remote, "error")) {
    remote$type <- remote$type %||% "github"
    return(remote)
  }

  remote <- catch(renv_remotes_parse_gitssh(spec))
  if (!inherits(remote, "error")) {
    remote$type <- remote$type %||% "git"
    return(remote)
  }

  remote <- catch(renv_remotes_parse_url(spec))
  if (!inherits(remote, "error")) {
    remote$type <- remote$type %||% "url"
    return(remote)
  }

  remote <- catch(renv_remotes_parse_git(spec))
  if (!inherits(remote, "error")) {
    remote$type <- remote$type %||% "git"
    return(remote)
  }

  stopf("failed to parse remote spec '%s'", spec)

}

renv_remotes_resolve_bioc_version <- function(version) {

  # initialize Bioconductor
  renv_bioconductor_init()
  BiocManager <- renv_scope_biocmanager()

  # handle versions like 'release' and 'devel'
  versions <- BiocManager$.version_map()
  row <- versions[versions$BiocStatus == version, ]
  if (nrow(row))
    return(row$Bioc)

  # otherwise, use the default version
  BiocManager$version()

}

renv_remotes_resolve_bioc_plain <- function(remote) {

  list(
    Package = remote$package,
    Version = remote$version,
    Source  = "Bioconductor"
  )

}

renv_remotes_resolve_bioc <- function(remote) {

  # if we parsed this as a repository remote, use that directly
  if (!is.null(remote$package))
    return(renv_remotes_resolve_bioc_plain(remote))

  # otherwise, this was parsed as a regular remote, declaring the package
  # should be obtained from a particular Bioconductor release
  package <- remote$repo
  biocversion <- renv_remotes_resolve_bioc_version(remote$user)
  biocrepos <- renv_bioconductor_repos(version = biocversion)
  record <- renv_available_packages_latest(package, repos = biocrepos)

  # update fields
  record$Source <- "Bioconductor"
  record$Repository <- NULL

  # return the resolved record
  record

}

renv_remotes_resolve_bitbucket <- function(remote) {

  user   <- remote$user
  repo   <- remote$repo
  subdir <- remote$subdir
  ref    <- remote$ref %||% getOption("renv.bitbucket.default_branch", "master")

  host <- remote$host %||% config$bitbucket.host()

  # scope authentication
  renv_scope_auth(repo)

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

renv_remotes_resolve_repository <- function(remote, latest) {

  package <- remote$package
  if (package %in% renv_packages_base())
    return(renv_remotes_resolve_base(package))

  version <- remote$version
  repository <- remote$repository

  if (latest && is.null(version)) {
    remote <- renv_available_packages_latest(package)
    version <- remote$Version
    repository <- remote$Repository
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

  # scope authentication
  renv_scope_auth(repo)

  # make request
  fmt <- "%s/repos/%s/%s/pulls/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, pull)
  jsonfile <- renv_scope_tempfile("renv-json-")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)

  # read resulting JSON
  json <- renv_json_read(jsonfile)
  json$head$sha

}

renv_remotes_resolve_github_sha_ref <- function(host, user, repo, ref) {

  # scope authentication
  renv_scope_auth(repo)

  # build url for github commits endpoint
  fmt <- "%s/repos/%s/%s/commits/%s"
  origin <- renv_retrieve_origin(host)

  ref <- ref %||% renv_remotes_resolve_github_ref(host, user, repo)
  url <- sprintf(fmt, origin, user, repo, ref %||% "main")

  # prepare headers
  headers <- c(Accept = "application/vnd.github.sha")

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

renv_remotes_resolve_github_modules <- function(host, user, repo, subdir, sha) {

  # form path to .gitmodules file
  subdir <- subdir %||% ""
  parts <- c(
    if (nzchar(subdir)) URLencode(subdir),
    ".gitmodules"
  )

  path <- paste(parts, collapse = "/")

  # scope authentication
  renv_scope_auth(repo)

  # add headers
  headers <- c(Accept = "application/vnd.github.raw")

  # get the file contents
  fmt <- "%s/repos/%s/%s/contents/%s?ref=%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, path, sha)
  jsonfile <- renv_scope_tempfile("renv-json-")
  status <- suppressWarnings(
    catch(
      download(url, destfile = jsonfile, type = "github", quiet = TRUE, headers = headers)
    )
  )

  # just return a status code whether or not submodules are included
  !inherits(status, "error")

}

renv_remotes_resolve_github_description <- function(url, host, user, repo, subdir, sha) {

  # form DESCRIPTION path
  subdir <- subdir %||% ""
  parts <- c(
    if (nzchar(subdir)) URLencode(subdir),
    "DESCRIPTION"
  )

  descpath <- paste(parts, collapse = "/")

  # scope authentication
  renv_scope_auth(repo)

  # add headers
  headers <- c(
    Accept = "application/vnd.github.raw",
    renv_download_auth_github(url)
  )

  # get the DESCRIPTION contents
  fmt <- "%s/repos/%s/%s/contents/%s?ref=%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, descpath, sha)
  destfile <- renv_scope_tempfile("renv-json-")
  download(url, destfile = destfile, type = "github", quiet = TRUE, headers = headers)

  # try to read the file; detect JSON versus raw content in case
  # headers were not sent for some reason
  contents <- renv_file_read(destfile)
  if (substring(contents, 1L, 1L) == "{") {
    json <- renv_json_read(text = contents)
    contents <- renv_base64_decode(json$content)
  }

  # normalize newlines
  contents <- gsub("\r\n", "\n", contents, fixed = TRUE)

  # read as DCF
  renv_dcf_read(text = contents)

}

renv_remotes_resolve_github_ref <- function(host, user, repo) {

  tryCatch(
    renv_remotes_resolve_github_ref_impl(host, user, repo),
    error = function(e) {
      warning(e)
      getOption("renv.github.default_branch", default = "main")
    }
  )

}

renv_remotes_resolve_github_ref_impl <- function(host, user, repo) {

  # scope authentication
  renv_scope_auth(repo)

  # build url to repos endpoint
  fmt <- "%s/repos/%s/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo)

  # download JSON data at endpoint
  jsonfile <- renv_scope_tempfile("renv-github-ref-", fileext = ".json")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)

  # read default branch
  json$default_branch %||% getOption("renv.github.default_branch", default = "main")

}

renv_remotes_resolve_github <- function(remote) {

  # resolve the reference associated with this repository
  host   <- remote$host %||% config$github.host()
  user   <- remote$user
  repo   <- remote$repo
  spec   <- remote$spec
  subdir <- remote$subdir

  # resolve ref
  ref <- remote$ref %||% renv_remotes_resolve_github_ref(host, user, repo)

  # handle '*release' refs
  if (identical(ref, "*release"))
    ref <- renv_remotes_resolve_github_release(host, user, repo, spec)

  # resolve the sha associated with the ref / pull
  pull   <- remote$pull %||% ""
  sha <- case(
    nzchar(pull) ~ renv_remotes_resolve_github_sha_pull(host, user, repo, pull),
    nzchar(ref)  ~ renv_remotes_resolve_github_sha_ref(host, user, repo, ref)
  )

  # if an abbreviated sha was provided as the ref, expand it here
  if (nzchar(ref) && startsWith(sha, ref))
    ref <- sha

  # check whether the repository has a .gitmodules file; if so, then we'll have
  # to use a plain 'git' client to retrieve the package
  modules <- renv_remotes_resolve_github_modules(host, user, repo, subdir, sha)

  # construct full url
  origin <- fsub("api.github.com", "github.com", renv_retrieve_origin(host))
  parts <- c(origin, user, repo)
  url <- paste(parts, collapse = "/")

  # read DESCRIPTION
  desc <- renv_remotes_resolve_github_description(url, host, user, repo, subdir, sha)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = if (modules) "git" else "GitHub",
    RemoteType     = if (modules) "git" else "github",
    RemoteUrl      = if (modules) url,
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha
  )

}

renv_remotes_resolve_github_release <- function(host, user, repo, spec) {

  # scope authentication
  renv_scope_auth(repo)

  # build url for github releases endpoint
  fmt <- "%s/repos/%s/%s/releases?per_page=1"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo)

  # prepare headers
  headers <- c(Accept = "application/vnd.github.raw+json")

  # make request to endpoint
  releases <- renv_scope_tempfile("renv-releases-")
  download(
    url      = url,
    destfile = releases,
    type     = "github",
    quiet    = TRUE,
    headers  = headers
  )

  # get reference associated with this tag
  json <- renv_json_read(releases)
  if (empty(json)) {
    fmt <- "could not find any releases associated with remote '%s'"
    stopf(fmt, sub("[*]release$", "", spec))
  }

  json[[1L]][["tag_name"]]

}

renv_remotes_resolve_git <- function(remote) {

  package <- remote$package %||% basename(remote$repo)
  url     <- remote$url
  subdir  <- remote$subdir

  # handle git ref
  pull <- remote$pull %||% ""
  ref  <- remote$ref %||% ""

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

  origin <- record$RemoteUrl
  ref <- record$RemoteRef %||% record$RemoteSha
  args <- c("ls-remote", origin, ref)

  output <- local({
    renv_scope_auth(record)
    renv_scope_git_auth()
    renv_system_exec("git", args, "checking git remote")
  })

  if (empty(output))
    return("")

  # format of output is, for example:
  #
  #   $ git ls-remote https://github.com/rstudio/renv refs/tags/0.14.0
  #   20ca74bdcc3c87848e5665effa2fc8ee8b039c69        refs/tags/0.14.0
  #
  # take first line of output, split on tab character, and take leftmost entry
  strsplit(output[[1L]], "\t", fixed = TRUE)[[1L]][[1L]]

}


renv_remotes_resolve_git_description <- function(record) {

  path <- renv_scope_tempfile("renv-git-")
  ensure_directory(path)

  # TODO: is there a cheaper way for us to accomplish this?
  # it'd be nice if we could retrieve the contents of a single
  # file, without needing to pull an entire repository branch
  local({
    renv_scope_options(renv.verbose = FALSE)
    renv_retrieve_git_impl(record, path)
  })

  # subdir may be NULL
  subdir <- record$RemoteSubdir
  desc <- renv_description_read(path, subdir = subdir)

  desc
}

renv_remotes_resolve_git_pull <- function(pr) {
  fmt <- "pull/%1$s/head:pull/%1$s"
  sprintf(fmt, pr)
}

renv_remotes_resolve_gitlab_ref <- function(host, user, repo) {

  tryCatch(
    renv_remotes_resolve_gitlab_ref_impl(host, user, repo),
    error = function(e) {
      warning(e)
      getOption("renv.gitlab.default_branch", default = "master")
    }
  )

}

renv_remotes_resolve_gitlab_ref_impl <- function(host, user, repo) {

  # scope authentication
  renv_scope_auth(repo)

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

renv_remotes_resolve_gitlab <- function(remote) {

  host   <- remote$host %||% config$gitlab.host()
  user   <- remote$user
  repo   <- remote$repo
  subdir <- remote$subdir %||% ""

  ref <- remote$ref %||% renv_remotes_resolve_gitlab_ref(host, user, repo)

  parts <- c(if (nzchar(subdir)) subdir, "DESCRIPTION")
  descpath <- URLencode(paste(parts, collapse = "/"), reserved = TRUE)

  # scope authentication
  renv_scope_auth(repo)

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

renv_remotes_resolve_url <- function(url, quiet = FALSE) {

  tempfile <- renv_scope_tempfile("renv-url-")
  writeLines(url, con = tempfile)
  hash <- md5sum(tempfile)

  ext <- fileext(url, default = ".tar.gz")
  name <- paste(hash, ext, sep = "")
  path <- renv_paths_source("url", name)

  ensure_parent_directory(path)
  download(url, path, quiet = quiet)

  desc <- renv_description_read(path)

  list(
    Package    = desc$Package,
    Version    = desc$Version,
    Source     = "URL",
    RemoteType = "url",
    RemoteUrl  = url,
    Path       = path
  )

}

renv_remotes_resolve_path <- function(path) {

  # if this package lives within one of the cellar paths,
  # then treat it as a cellar source
  roots <- renv_cellar_roots()
  for (root in roots)
    if (renv_path_within(path, root))
      return(renv_remotes_resolve_path_cellar(path))

  # first, check for a common extension
  if (renv_archive_type(path) %in% c("tar", "zip"))
    return(renv_remotes_resolve_path_impl(path))

  # otherwise, if this is the path to a package project, use the sources as-is
  if (renv_project_type(path) == "package")
    return(renv_remotes_resolve_path_impl(path))

  stopf("there is no package at path '%s'", path)

}

renv_remotes_resolve_path_cellar <- function(path) {

  desc <- renv_description_read(path)
  list(
    Package    = desc$Package,
    Version    = desc$Version,
    Source     = "Cellar",
    Cacheable  = FALSE
  )

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
