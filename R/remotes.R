
renv_remotes_parse <- function(entry) {

  # check for URLs
  if (grepl("^(?:file|https?)://", entry))
    return(renv_remotes_parse_url(entry))

  # check for paths to existing local files
  record <- catch(renv_remotes_parse_local(entry))
  if (!inherits(record, "error"))
    return(record)

  # check for pre-supplied type
  type <- NULL
  parts <- strsplit(entry, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    type <- parts[[1]]
    entry <- parts[[2]]
  }

  # if we don't have at type, infer from entry (can be either CRAN or GitHub)
  type <- type %||% if (grepl("/", entry)) "github" else "cran"

  # generate entry from type
  switch(type,
    cran   = renv_remotes_parse_cran(entry),
    github = renv_remotes_parse_github(entry),
    stopf("unknown remote type '%s'", type %||% "<NA>")
  )

}

renv_remotes_parse_cran <- function(entry) {

  parts <- strsplit(entry, "@", fixed = TRUE)[[1]]
  package <- parts[1]
  version <- parts[2]

  # if no version was provided, take this as a request
  # to use the latest version available on CRAN
  if (is.na(version) || identical(version, "latest")) {
    record <- catch(renv_records_cran_latest(package))
    if (!inherits(record, "error"))
      return(record)
  }

  # otherwise, honor the requested version
  list(
    Package = package,
    Version = version %NA% NULL,
    Source = "CRAN"
  )

}

renv_remotes_parse_github_sha_pull <- function(host, user, repo, pull) {

  fmt <- "https://%s/repos/%s/%s/pulls/%s"
  url <- sprintf(fmt, host, user, repo, pull)
  jsonfile <- renv_tempfile("ren-json-")
  download(url, destfile = jsonfile, quiet = TRUE)
  json <- renv_json_read(jsonfile)
  json$head$sha

}

renv_remotes_parse_github_sha_ref <- function(host, user, repo, ref) {

  fmt <- "https://%s/repos/%s/%s/commits/%s"
  url <- sprintf(fmt, host, user, repo, ref)
  headers <- c(Accept = "application/vnd.github.v2.sha")
  shafile <- renv_tempfile("renv-sha-")
  download(url, destfile = shafile, quiet = TRUE, headers = headers)
  sha <- readChar(shafile, file.info(shafile)$size, TRUE)

  # check for JSON response (in case our headers weren't sent)
  if (nchar(sha) > 40) {
    json <- renv_json_read(text = sha)
    sha <- json$sha
  }

  sha

}

renv_remotes_parse_github <- function(entry) {

  pattern <- "^([^/]+)/([^@#]+)(?:#(.*))?(?:@(.*))?"
  matches <- regexec(pattern, entry)
  parts <- regmatches(entry, matches)[[1]]

  user <- parts[2]
  repo <- parts[3]
  pull <- parts[4]
  ref  <- parts[5] %""% "master"

  # TODO: configure GitHub host on a per-package or per-remote basis?
  host <- renv_config("github.host", "api.github.com")

  # resolve the sha associated with the ref / pull
  sha <- case(
    nzchar(pull) ~ renv_remotes_parse_github_sha_pull(host, user, repo, pull),
    nzchar(ref)  ~ renv_remotes_parse_github_sha_ref(host, user, repo, ref)
  )

  # get the DESCRIPTION contents
  fmt <- "https://%s/repos/%s/%s/contents/DESCRIPTION?ref=%s"
  url <- sprintf(fmt, host, user, repo, sha)
  jsonfile <- renv_tempfile("renv-json-")
  download(url, destfile = jsonfile, quiet = TRUE)
  json <- renv_json_read(jsonfile)
  contents <- renv_base64_decode(json$content)

  descfile <- renv_tempfile("renv-description-")
  writeLines(contents, con = descfile)
  desc <- renv_dcf_read(descfile)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "GitHub",
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteRef      = ref,
    RemoteSha      = sha,
    RemoteHost     = host
  )

}

renv_remotes_parse_url <- function(entry) {

  tempfile <- renv_tempfile("renv-url-")
  writeLines(entry, con = tempfile)
  hash <- tools::md5sum(tempfile)

  ext <- fileext(entry, default = ".tar.gz")
  name <- paste(hash, ext, sep = "")
  path <- renv_paths_source("url", name)

  ensure_parent_directory(path)
  download(entry, path)

  desc <- renv_description_read(path)

  list(
    Package   = desc$Package,
    Version   = desc$Version,
    Source    = "URL",
    Path      = path,
    RemoteUrl = entry
  )

}

renv_remotes_parse_local <- function(entry) {

  # check for existing path
  path <- normalizePath(entry, winslash = "/", mustWork = TRUE)

  # first, check for a common extension
  ext <- fileext(entry)
  if (ext %in% c(".tar.gz", ".tgz", ".zip"))
    return(renv_remotes_parse_local_impl(path))

  # otherwise, if this is the path to a package project, use the sources as-is
  if (renv_project_type(path) == "package")
    return(renv_remotes_parse_local_impl(path))

  stopf("there is no package at path '%s'", entry)

}

renv_remotes_parse_local_impl <- function(path) {

  desc <- renv_description_read(path)
  list(
    Package   = desc$Package,
    Version   = desc$Version,
    Source    = path,
    Cacheable = FALSE
  )

}
