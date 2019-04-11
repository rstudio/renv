
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

  # get the sha associated with this ref
  fmt <- "https://api.github.com/repos/%s/%s/commits/%s"
  url <- sprintf(fmt, user, repo, ref)
  shafile <- renv_tempfile("renv-sha-")

  headers <- c(Accept = "application/vnd.github.v2.sha")
  download(url, destfile = shafile, quiet = TRUE, headers = headers)
  sha <- readChar(shafile, file.info(shafile)$size, TRUE)

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

renv_remotes_snapshot <- function(project, libpaths = NULL) {

  # resolve variables
  libpaths <- libpaths %||% renv_remotes_libpaths()

  # serialize descs for installed packages
  entries <- uapply(libpaths, function(libpath) {
    packages <- list.files(libpath, full.names = TRUE)
    descs <- file.path(packages, "desc")
    map_chr(descs, renv_remotes_serialize)
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

renv_remotes_serialize <- function(desc) {

  if (!file.exists(desc))
    return(NULL)

  if (is.character(desc))
    desc <- renv_description_read(desc)

  # infer the remote type
  type <- tolower(desc$RemoteType) %||% ""
  switch(type,
    cran     = renv_remotes_serialize_cran(desc),
    github   = renv_remotes_serialize_github(desc),
    standard = renv_remotes_serialize_standard(desc),
    url      = renv_remotes_serialize_url(desc),
    renv_remotes_serialize_unknown(desc, type)
  )

}

renv_remotes_serialize_cran <- function(desc) {
  package <- desc$Package
  version <- desc$Version
  sprintf("%s@%s", package, version)
}

renv_remotes_serialize_github <- function(desc) {
  user <- desc$RemoteUsername
  repo <- desc$RemoteRepo
  ref  <- desc$RemoteSha %||% desc$RemoteRef
  sprintf("%s/%s@%s", user, repo, ref)
}

renv_remotes_serialize_standard <- function(desc) {
  package <- desc$Package
  version <- desc$Version
  sprintf("%s@%s", package, version)
}

renv_remotes_serialize_url <- function(desc) {
  desc$RemoteUrl
}

renv_remotes_serialize_unknown <- function(desc, type) {

  # if we have a repository field, assume CRAN
  if (!is.null(desc$Repository))
    return(renv_remotes_serialize_cran(desc))

  # TODO: alternate form?
  renv_remotes_serialize_standard(desc)

}
