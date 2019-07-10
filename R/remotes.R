
renv_remotes_parse <- function(entry) {

  # check for URLs
  if (grepl("^(?:file|https?)://", entry))
    return(renv_remotes_parse_url(entry))

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

renv_remotes_parse_github <- function(entry) {

  parts <- strsplit(entry, "[@/]")[[1]]
  if (length(parts) < 2)
    stopf("invalid github remotes specification '%s'", entry)

  user <- parts[1]
  repo <- parts[2]
  ref  <- parts[3] %NA% "master"

  # get the sha associated with this ref
  fmt <- "https://api.github.com/repos/%s/%s/commits/%s"
  url <- sprintf(fmt, user, repo, ref)
  shafile <- renv_tempfile("renv-sha-")

  headers <- c(Accept = "application/vnd.github.v2.sha")
  download(url, destfile = shafile, quiet = TRUE, headers = headers)
  sha <- readChar(shafile, file.info(shafile)$size, TRUE)

  # check for JSON response (in case where our headers weren't sent)
  if (nchar(sha) > 40) {
    json <- renv_json_read(text = sha)
    sha <- json$sha
  }

  # get the DESCRIPTION contents
  fmt <- "https://raw.githubusercontent.com/%s/%s/%s/DESCRIPTION"
  url <- sprintf(fmt, user, repo, sha)
  descfile <- renv_tempfile("renv-description-")
  download(url, destfile = descfile, quiet = TRUE)
  desc <- renv_description_read(descfile)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "GitHub",
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteRef      = ref,
    RemoteSha      = sha,
    RemoteHost     = "api.github.com"
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
