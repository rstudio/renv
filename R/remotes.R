
# TODO: get rid of all of this once the requisite APIs become part of remotes
renv_remotes_read <- function(remotes = NULL) {
  remotes <- remotes %||% renv_remotes_path()
  contents <- readLines(remotes, warn = FALSE)
  parsed <- lapply(contents, renv_remotes_parse)
  names(parsed) <- map_chr(parsed, `[[`, "Package")
  parsed
}

renv_remotes_parse <- function(entry) {

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
  version <- parts[2] %NA% NULL
  list(Package = package, Version = version, Source = "CRAN")
}

renv_remotes_parse_github <- function(entry) {

  parts <- strsplit(entry, "[@/]")[[1]]
  if (length(parts) < 2)
    stopf("invalid github remotes specification '%s'", entry)

  user <- parts[1]
  repo <- parts[2]
  ref  <- parts[3] %NA% "master"

  # first, resolve a sha that's just a reference
  fmt <- "https://api.github.com/repos/%s/%s/git/refs/heads/%s"
  url <- sprintf(fmt, user, repo, ref)
  jsonfile <- tempfile(pattern = "renv-github-ref-", fileext = ".json")
  on.exit(unlink(jsonfile), add = TRUE)
  download(url, destfile = jsonfile, quiet = TRUE)

  json <- renv_json_read(jsonfile)[[1]]
  sha <- json$object$sha

  # now, get the DESCRIPTION contents
  fmt <- "https://raw.githubusercontent.com/%s/%s/%s/DESCRIPTION"
  url <- sprintf(fmt, user, repo, sha)
  descfile <- tempfile()
  download(url, destfile = descfile, quiet = TRUE)
  on.exit(unlink(descfile), add = TRUE)

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

renv_remotes_snapshot <- function(project, libpaths = NULL) {

  # resolve variables
  libpaths <- libpaths %||% renv_remotes_libpaths()

  # serialize DESCRIPTIONs for installed packages
  entries <- uapply(libpaths, function(libpath) {
    packages <- list.files(libpath, full.names = TRUE)
    descriptions <- file.path(packages, "DESCRIPTION")
    map_chr(descriptions, renv_remotes_serialize)
  })

  # write to remotes.txt
  writeLines(entries, con = renv_remotes_path())

}

renv_remotes_path <- function(project = NULL) {
  project <- project %||% renv_project()
  file.path(project, "remotes.txt")
}

renv_remotes_libpaths <- function(libpaths = NULL) {
  setdiff(renv_libpaths_all(), normalizePath(.Library, winslash = "/"))
}

renv_remotes_serialize <- function(description) {

  if (!file.exists(description))
    return(NULL)

  if (is.character(description))
    description <- renv_description_read(description)

  # infer the remote type
  type <- tolower(description$RemoteType) %||% ""
  switch(type,
    cran     = renv_remotes_serialize_cran(description),
    github   = renv_remotes_serialize_github(description),
    standard = renv_remotes_serialize_standard(description),
    url      = renv_remotes_serialize_url(description),
    renv_remotes_serialize_unknown(description, type)
  )

}

renv_remotes_serialize_cran <- function(description) {
  with(description, {
    sprintf("%s@%s", Package, Version)
  })
}

renv_remotes_serialize_github <- function(description) {
  with(description, {
    sprintf("%s/%s@%s", RemoteUsername, RemoteRepo, RemoteSha)
  })
}

renv_remotes_serialize_standard <- function(description) {
  with(description, {
    sprintf("%s@%s", Package, Version)
  })
}

renv_remotes_serialize_url <- function(description) {
  with(description, {
    sprintf("%s", RemoteUrl)
  })
}

renv_remotes_serialize_unknown <- function(description, type) {

  # if we have a repository field, assume CRAN
  if (!is.null(description$Repository))
    return(renv_remotes_serialize_cran(description))

  # otherwise, write as unknown
  with(description, {
    sprintf("%s@%s", Package, Version)
  })

}
