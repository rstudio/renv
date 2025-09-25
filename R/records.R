
renv_records_select <- function(records, actions, action) {
  records <- renv_lockfile_records(records)
  matching <- actions[actions %in% action]
  keep(records, names(matching))
}

renv_records_sort <- function(records) {
  records[csort(names(records))]
}

renv_records_override <- function(records) {
  enumerate(records, renv_options_override, scope = "renv.records")
}

renv_record_names <- function(record, fields = NULL) {
  fields <- fields %||% c("Package", "Version", "Source")
  remotes <- grep("^Remote", names(record), value = TRUE)
  nms <- c(fields, setdiff(remotes, "Remotes"))
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

  # if this appears to be a file path, then keep it as-is
  source <- record$Source %||% "unknown"
  if (grepl("[/\\]", source))
    return(source)

  # otherwise, try to normalize it
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
    if (tolower(repos) %in% c("bioc", "bioconductor"))
      source <- "bioconductor"
  }

  # all done; return normalized source
  source

}

renv_record_validate <- function(package, record) {

  # check for a record -- minimally, a list with a package name
  if (is.list(record) && is.character(record$Package))
    return(record)

  # if we're running tests, or in CI, then report
  if (renv_tests_running() || renv_envvar_exists("CI")) {
    fmt <- "! Internal error: unexpected record for package '%s'"
    writef(fmt, package)
    print(record)
  }

  # return record as-is
  record

}

renv_record_format_remote <- function(record,
                                      compact = FALSE,
                                      versioned = TRUE,
                                      pak = FALSE)
{
  # if we have a pkgref, we can use that directly
  pkgref <- record$RemotePkgRef
  if (!is.null(pkgref))
    return(pkgref)

  # extract some of the commonly used fields up-front
  source <- renv_record_source(record, normalize = TRUE)
  package <- record[["Package"]]
  version <- record[["Version"]]

  # handle repository remotes
  if (source %in% c("cran", "repository", "standard")) {
    parts <- c(package, if (versioned) version)
    remote <- paste(parts, collapse = "@")
    return(remote)
  }

  # handle bioconductor remotes
  if (source %in% "bioconductor") {
    parts <- c(package, if (versioned) version)
    suffix <- paste(parts, collapse = "@")
    remote <- paste("bioc", suffix, sep = "::")
    return(remote)
  }

  # handle git, svn remotes
  if (source %in% c("git", "svn")) {
    url <- record[["RemoteUrl"]]
    remote <- sprintf("%s::%s", source, url)
    return(remote)
  }

  # handle local, url remotes
  if (source %in% c("local", "url")) {
    url <- record[["RemoteUrl"]] %||% sub("[^:]+::", "", record[["RemotePkgRef"]])
    remote <- sprintf("%s=%s::%s", package, source, url)
    return(remote)
  }

  # handle other remotes; assumed to be a github-like remote
  host   <- record[["RemoteHost"]]
  user   <- record[["RemoteUsername"]]
  repo   <- record[["RemoteRepo"]]
  type   <- record[["RemoteType"]]
  ref    <- record[["RemoteRef"]]
  sha    <- record[["RemoteSha"]]
  subdir <- record[["RemoteSubdir"]]
  subdirsep <- if (pak) "/" else ":"

  # skip type and host if they're defaults
  if (identical(type, "github")) {
    if (is.null(host) || identical(host, "api.github.com")) {
      type <- NULL
      host <- NULL
    }
  }

  # omit gitlab host as well for default host
  if (identical(type, "gitlab")) {
    if (identical(host, "gitlab.com")) {
      host <- NULL
    }
  }

  # skip 'boring' refs for compact display
  if (compact) {
    if (length(ref) && ref %in% c("HEAD", "main", "master")) {
      ref <- NULL
      sha <- NULL
    }
  }

  # start building the remote specification
  stk <- stack(mode = "character")

  if (!identical(package, repo))
    stk$push(package, "=")

  if (!is.null(type)) {
    stk$push(type)
    if (!is.null(host) && nzchar(host))
      stk$push("@", host)
    stk$push("::")
  }

  stk$push(user, "/", repo)

  if (!is.null(subdir) && nzchar(subdir))
    stk$push(subdirsep, subdir)

  if (versioned) {

    if (pak) {
      stk$push("@", sha %||% ref %||% "HEAD")
    } else if (length(sha)) {
      stk$push("@", if (compact) substring(sha, 1L, 8L) else sha)
    } else if (length(ref)) {
      stk$push("@", ref)
    }

  } else {

    if (length(ref))
      stk$push("@", ref)

  }

  remote <- paste(stk$data(), collapse = "")
  return(remote)
}

renv_record_format_short <- function(record, versioned = FALSE) {

  if (is.null(record))
    return(renv_record_placeholder())

  remotes <- c("RemoteUsername", "RemoteRepo")
  if (all(remotes %in% names(record))) {
    remote <- renv_record_format_remote(record, compact = TRUE)
    if (versioned)
      remote <- sprintf("%s  [%s]", record$Version %||% "<NA>", remote)
    return(remote)
  }

  record$Version

}

renv_record_format_pair <- function(lhs, rhs, separator = "->") {

  placeholder <- renv_record_placeholder()

  # check for install / remove
  if (is.null(lhs) || is.null(rhs)) {
    lhs <- renv_record_format_short(lhs)
    rhs <- renv_record_format_short(rhs)
    return(sprintf("[%s %s %s]", lhs, separator, rhs))
  }

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

  # check for CRAN packages; in such cases, we typically want to ignore
  # the Remote fields which might've been added by 'pak' or other tools
  isrepo <-
    nzchar(lhs$Version %||% "") &&
    nzchar(rhs$Version %||% "") &&
    nzchar(lhs$Repository %||% "") &&
    nzchar(rhs$Repository %||% "") &&
    identical(lhs$Repository, rhs$Repository)

  if (isrepo) {
    fmt <- "[%s %s %s]"
    lhsf <- renv_record_format_short(lhs)
    rhsf <- renv_record_format_short(rhs)
    return(sprintf(fmt, lhsf, separator, rhsf))
  }

  # check for only sha changed
  usesha <-
    setequal(changed, "RemoteSha") ||
    setequal(changed, c("RemoteSha", "Version"))

  if (usesha) {

    user <- lhs$RemoteUsername %||% placeholder
    repo <- lhs$RemoteRepo %||% placeholder
    spec <- paste(user, repo, sep = "/")

    ref <- lhs$RemoteRef %||% placeholder
    if (!ref %in% c("main", "master", "*"))
      spec <- paste(spec, ref, sep = "@")

    fmt <- "[%s: %s %s %s]"
    lsha <- substring(lhs$RemoteSha %||% placeholder, 1L, 8L)
    rsha <- substring(rhs$RemoteSha %||% placeholder, 1L, 8L)

    return(sprintf(fmt, spec, lsha, separator, rsha))

  }

  # check for only source change
  if (setequal(changed, "Source")) {
    fmt <- "[%s: %s %s %s]"
    ver <- lhs$Version %||% placeholder
    lhsf <- lhs$Source %||% placeholder
    rhsf <- rhs$Source %||% placeholder
    return(sprintf(fmt, ver, lhsf, separator, rhsf))
  }

  # check only version changed
  if (setequal(changed, "Version")) {
    fmt <- "[%s %s %s]"
    lhsf <- lhs$Version %||% placeholder
    rhsf <- rhs$Version %||% placeholder
    return(sprintf(fmt, lhsf, separator, rhsf))
  }

  # if the source has changed, highlight that
  if ("Source" %in% changed) {
    fmt <- "[%s %s %s]"
    lhsf <- renv_record_format_short(lhs)
    rhsf <- renv_record_format_short(rhs)
    return(sprintf(fmt, lhsf, separator, rhsf))
  }

  # otherwise, report each diff individually
  diffs <- map_chr(changed, function(field) {

    lhsf <- lhs[[field]] %||% placeholder
    rhsf <- rhs[[field]] %||% placeholder

    if (field == "RemoteSha") {
      lhsf <- substring(lhsf, 1L, 8L)
      rhsf <- substring(rhsf, 1L, 8L)
    }

    fmt <- "%s: %s %s %s"
    sprintf(fmt, map[[field]], lhsf, separator, rhsf)
  })

  sprintf("[%s]", paste(diffs, collapse = "; "))

}

renv_records_equal <- function(lhs, rhs) {

  lhs <- reject(lhs, is.null)
  rhs <- reject(rhs, is.null)

  nm <- setdiff(union(names(lhs), names(rhs)), "Hash")
  identical(keep(lhs, nm), keep(rhs, nm))

}

renv_records_resolve <- function(records, latest = FALSE) {

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
    renv_remotes_resolve(record, latest)

  })

}
