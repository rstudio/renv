
# tools for interacting with the R package records encoded
# within a lockfile
renv_records <- function(records) {
  if (inherits(records, "renv_lockfile"))
    return(records$Packages)
  records
}

`renv_records<-` <- function(x, value) {
  x$Packages <- filter(value, Negate(empty))
  x
}

renv_records_select <- function(records, actions, action) {
  records <- renv_records(records)
  matching <- actions[actions %in% action]
  keep(records, names(matching))
}

renv_records_sort <- function(records) {
  renv_scope_locale("LC_COLLATE", "C")
  records[order(names(records))]
}

renv_records_override <- function(records) {
  enumerate(records, renv_options_override, scope = "renv.records")
}

renv_record_names <- function(record, fields = NULL) {
  fields <- fields %||% c("Package", "Version", "Source")
  remotes <- grep("^Remote", names(record), value = TRUE)
  nms <- c(fields, remotes)
  renv_vector_intersect(nms, names(record))
}

renv_record_cacheable <- function(record) {

  # check if the record has been marked as cacheable
  cacheable <- record$Cacheable %||% TRUE
  if (identical(cacheable, FALSE))
    return(FALSE)

  # check for unknown source
  source <- renv_record_source(record)
  if (source == "unknown")
    return(FALSE)

  # record is ok
  TRUE

}

renv_record_source <- function(record, normalize = FALSE) {
  source <- tolower(record$Source %||% "unknown")
  if (normalize)
    source <- renv_record_source_normalize(record, source)
  source
}

renv_record_source_normalize <- function(record, source) {

  # normalize different types of git remotes
  if (source %in% c("git2r", "xgit"))
    source <- "git"

  # handle old lockfiles where 'source' was explicitly set as CRAN
  if (source %in% c("cran"))
    source <- "repository"

  # check for ad-hoc requests to install from bioc
  if (identical(source, "repository")) {
    repos <- record$Repository %||% ""
    if (repos %in% c("bioc", "bioconductor"))
      source <- "bioconductor"
  }

  # all done; return normalized source
  source

}

renv_record_validate <- function(record, quiet = FALSE) {

  # TODO
  TRUE

}

renv_record_format_short <- function(record) {

  remotes <- c("RemoteUsername", "RemoteRepo")
  if (all(remotes %in% names(record)))
    return(renv_record_format_short_remote(record))

  # TODO: include other information (e.g. repository name)?
  record$Version

}

renv_record_format_short_remote <- function(record) {

  text <- paste(record$RemoteUsername, record$RemoteRepo, sep = "/")

  subdir <- record$RemoteSubdir %||% ""
  if (nzchar(subdir))
    text <- paste(text, subdir, sep = ":")

  if (!is.null(record$RemoteRef)) {
    ref <- record$RemoteRef
    if (!identical(ref, "master"))
      text <- paste(text, record$RemoteRef, sep = "@")
  } else if (!is.null(record$RemoteSha)) {
    sha <- substring(record$RemoteSha, 1L, 8L)
    text <- paste(text, sha, sep = "@")
  }

  text

}

renv_record_format_pair <- function(lhs, rhs) {

  # check for install / remove
  if (is.null(lhs))
    return(sprintf("[* -> %s]", renv_record_format_short(rhs)))
  else if (is.null(rhs))
    return(sprintf("[%s -> *]", renv_record_format_short(lhs)))

  map <- list(
    Source         = "src",
    Repository     = "repo",
    Version        = "ver",
    RemoteHost     = "host",
    RemoteUsername = "user",
    RemoteRepo     = "repo",
    RemoteRef      = "ref",
    RemoteSha      = "sha",
    RemoteSubdir   = "subdir"
  )

  fields <- names(map)

  # check to see which fields have changed between the two
  diff <- map_lgl(fields, function(field) {
    !identical(lhs[[field]], rhs[[field]])
  })

  changed <- names(which(diff))

  if (empty(changed)) {
    fmt <- "[%s: unchanged]"
    lhsf <- renv_record_format_short(lhs)
    return(sprintf(fmt, lhsf))
  }

  # check for only sha changed
  usesha <-
    setequal(changed, "RemoteSha") ||
    setequal(changed, c("RemoteSha", "Version"))

  if (usesha) {

    user <- lhs$RemoteUsername %||% "*"
    repo <- lhs$RemoteRepo %||% "*"
    spec <- paste(user, repo, sep = "/")

    ref <- lhs$RemoteRef %||% "*"
    if (!ref %in% c("master", "*"))
      spec <- paste(spec, ref, sep = "@")

    fmt <- "[%s: %s -> %s]"
    lsha <- substring(lhs$RemoteSha %||% "*", 1L, 8L)
    rsha <- substring(rhs$RemoteSha %||% "*", 1L, 8L)

    return(sprintf(fmt, spec, lsha, rsha))

  }

  # check for only source change
  if (setequal(changed, "Source")) {
    fmt <- "[%s: %s -> %s]"
    ver <- lhs$Version %||% "*"
    lhsf <- lhs$Source %||% "*"
    rhsf <- rhs$Source %||% "*"
    return(sprintf(fmt, ver, lhsf, rhsf))
  }

  # check only version changed
  if (setequal(changed, "Version")) {
    fmt <- "[%s -> %s]"
    lhsf <- lhs$Version %||% "*"
    rhsf <- rhs$Version %||% "*"
    return(sprintf(fmt, lhsf, rhsf))
  }

  # if the source has changed, highlight that
  if ("Source" %in% changed) {
    fmt <- "[%s -> %s]"
    lhsf <- renv_record_format_short(lhs)
    rhsf <- renv_record_format_short(rhs)
    return(sprintf(fmt, lhsf, rhsf))
  }

  # otherwise, report each diff individually
  diffs <- map_chr(changed, function(field) {

    lhsf <- lhs[[field]] %||% "*"
    rhsf <- rhs[[field]] %||% "*"

    if (field == "RemoteSha") {
      lhsf <- substring(lhsf, 1L, 8L)
      rhsf <- substring(rhsf, 1L, 8L)
    }

    fmt <- "%s: %s -> %s"
    sprintf(fmt, map[[field]], lhsf, rhsf)
  })

  sprintf("[%s]", paste(diffs, collapse = "; "))

}

renv_records_equal <- function(lhs, rhs) {

  lhs <- reject(lhs, is.null)
  rhs <- reject(rhs, is.null)

  nm <- setdiff(union(names(lhs), names(rhs)), "Hash")
  identical(keep(lhs, nm), keep(rhs, nm))

}

renv_records_resolve <- function(records) {

  enumerate(records, function(package, record) {

    # check for already-resolved records
    if (is.null(record) || is.list(record))
      return(record)

    # check for version-only specifications and
    # prepend the package name in such a case
    pattern <- "^(?:[[:digit:]]+[.-]){1,}[[:digit:]]+$"
    if (grepl(pattern, record))
      record <- paste(package, record, sep = "@")

    # resolve the record
    renv_remotes_resolve(record)

  })

}
