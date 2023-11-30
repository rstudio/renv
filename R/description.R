
renv_description_read <- function(path = NULL,
                                  package = NULL,
                                  subdir = NULL,
                                  field = NULL,
                                  ...)
{
  # if given a package name, construct path to that package
  path <- path %||% find.package(package)

  # normalize non-absolute paths
  if (!renv_path_absolute(path))
    path <- renv_path_normalize(path)

  # if 'path' refers to a directory, try to resolve the DESCRIPTION file
  if (dir.exists(path)) {
    components <- c(path, if (nzchar(subdir %||% "")) subdir, "DESCRIPTION")
    path <- paste(components, collapse = "/")
  }

  # if the DESCRIPTION file doesn't exist, bail
  if (!file.exists(path))
    stopf("DESCRIPTION file %s does not exist", renv_path_pretty(path))

  # read value with filebacked cache
  description <- filebacked(
    context  = "renv_description_read",
    path     = path,
    callback = renv_description_read_impl,
    subdir   = subdir,
    ...
  )

  if (!is.null(field))
    return(description[[field]])

  description

}

renv_description_read_impl <- function(path = NULL, subdir = NULL, ...) {

  # if we have an archive, attempt to unpack the DESCRIPTION
  type <- renv_archive_type(path)
  if (type != "unknown") {

    # list files within the archive
    files <- renv_archive_list(path)

    # find the DESCRIPTION file. note that for some source tarballs (e.g.
    # those from GitHub) the first entry may not be the package name, so
    # just consume everything up to the first slash
    subdir <- subdir %||% ""
    parts <- c("^[^/]+", if (nzchar(subdir)) subdir, "DESCRIPTION$")
    pattern <- paste(parts, collapse = "/")

    descs <- grep(pattern, files, value = TRUE)
    if (empty(descs)) {
      fmt <- "archive '%s' does not appear to contain a DESCRIPTION file"
      stopf(fmt, renv_path_aliased(path))
    }

    # choose the shortest DESCRPITION file matching
    # unpack into tempdir location
    file <- descs[[1]]
    exdir <- renv_scope_tempfile("renv-description-")
    renv_archive_decompress(path, files = file, exdir = exdir)

    # update path to extracted DESCRIPTION
    path <- file.path(exdir, file)

  }

  # read DESCRIPTION as dcf
  dcf <- renv_dcf_read(path, ...)
  if (empty(dcf))
    stopf("DESCRIPTION file at '%s' is empty", path)

  dcf

}

renv_description_path <- function(path) {
  childpath <- file.path(path, "DESCRIPTION")
  indirect <- file.exists(childpath)
  path[indirect] <- childpath[indirect]
  path
}

# parse the dependency requirements normally presented in
# Depends, Imports, Suggests, and so on
renv_description_parse_field <- function(field) {

  # check for invalid / unexpected inputs
  if (is.null(field) || is.na(field) || !nzchar(field))
    return(NULL)

  pattern <- paste0(
    "([a-zA-Z0-9._]+)",                      # package name
    "(?:\\s*\\(([><=]+)\\s*([0-9.-]+)\\))?"  # optional version specification
  )

  # split on commas
  parts <- strsplit(field, "\\s*,\\s*")[[1]]

  # drop any empty fields
  x <- parts[nzchar(parts)]

  # match to split on package name, version
  m <- regexec(pattern, x)
  matches <- regmatches(x, m)
  if (empty(matches))
    return(NULL)

  data_frame(
    Package = extract_chr(matches, 2L),
    Require = extract_chr(matches, 3L),
    Version = extract_chr(matches, 4L)
  )

}

renv_description_resolve <- function(path) {

  case(
    is.list(path)      ~ path,
    is.character(path) ~ renv_description_read(path = path)
  )

}

renv_description_built_version <- function(desc = NULL) {

  desc <- renv_description_resolve(desc)

  built <- desc[["Built"]]
  if (is.null(built))
    return(NA)

  substring(built, 3L, regexpr(";", built, fixed = TRUE) - 1L)
}

renv_description_dependency_fields_expand <- function(fields) {

  expanded <- map(fields, function(field) {

    case(

      identical(field, FALSE)
      ~ NULL,

      identical(field, "strong") || is.na(field)
      ~ c("Depends", "Imports", "LinkingTo"),

      identical(field, "most") || identical(field, TRUE)
      ~ c("Depends", "Imports", "LinkingTo", "Suggests"),

      identical(field, "all") ~
        c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),

      field

    )

  })

  unique(unlist(expanded, recursive = FALSE, use.names = FALSE))

}

renv_description_dependency_fields <- function(fields, project) {
  fields <- fields %||% settings$package.dependency.fields(project = project)
  renv_description_dependency_fields_expand(fields)
}

renv_description_remotes <- function(path) {

  desc <- catch(renv_description_read(path))
  if (inherits(desc, "error"))
    return(list())

  profile <- renv_profile_get()
  field <- if (is.null(profile))
    "Remotes"
  else
    sprintf("Config/renv/profiles/%s/remotes", profile)

  remotes <- desc[[field]]
  if (is.null(remotes))
    return(list())

  # if possible, resolve remotes lazily
  splat <- strsplit(remotes, "[[:space:]]*,[[:space:]]*")[[1]]
  remotes <- lapply(splat, function(spec) {

    # if this is a named remote, we can resolve it lazily
    idx <- c(regexpr("=", spec, fixed = TRUE))
    result <- if (idx == -1L) {
      remote <- renv_remotes_resolve(spec)
      list(Package = remote$Package, Remote = remote)
    } else {
      package <- substring(spec, 1L, idx - 1L)
      list(Package = package, Remote = function() renv_remotes_resolve(spec))
    }

  })

  # put together into named list
  records <- map(remotes, `[[`, 2L)
  names(records) <- map_chr(remotes, `[[`, 1L)
  records

}

