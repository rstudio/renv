
#' Update package records in a lockfile
#'
#' Use `record()` to record a new entry within an existing renv lockfile.
#'
#' This function can be useful when you need to change one or more of the
#' package records within an renv lockfile -- for example, because a recorded
#' package cannot be restored in a particular environment, and you know of a
#' suitable alternative.
#'
#' # Records
#'
#' Records can be provided either using the **remotes** short-hand syntax,
#' or by using an \R list of entries to record within the lockfile. See
#' `?lockfiles` for more information on the structure of a package record.
#'
#' @inheritParams renv-params
#'
#' @param records A list of named records, mapping package names to a definition
#'   of their source. See **Records** for more details.
#'
#' @param enrich Should resolved records be enriched with the DESCRIPTION
#'   fields that `snapshot()` would write (`Depends`, `Imports`, `Suggests`,
#'   `LinkingTo`, `License`, etc.)? Defaults to `TRUE`. When `TRUE`,
#'   `record()` consults the active package repositories (and crandb, when
#'   enabled, for CRAN packages) to fill in the additional fields, erroring
#'   if the remote source is unreachable. The `Hash` field is not computed
#'   for enriched records. Additional DESCRIPTION-only fields (`Title`,
#'   `Description`, `Author`, `Maintainer`) are only included when the
#'   source can supply them -- typically when crandb is reachable for CRAN
#'   packages, or when the source is a Git host (GitHub, GitLab, Bitbucket).
#'   Set to `FALSE` to keep the minimal record (`Package`, `Version`,
#'   `Source`, `Repository`) produced by remote resolution.
#'
#' @example examples/examples-record.R
#' @export
record <- function(records,
                   lockfile = NULL,
                   project  = NULL,
                   enrich   = TRUE)
{
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  lockfile <- lockfile %||% renv_lockfile_path(project)

  # track which entries came from a character spec; only those entries
  # are enriched, so caller-supplied list records pass through unchanged
  # (and offline, since enrichment otherwise requires a reachable source)
  if (is.character(records)) {
    enrichable <- rep_len(TRUE, length(records))
    records <- lapply(records, renv_remotes_resolve, latest = TRUE)
  } else if (is.list(records)) {
    enrichable <- map_lgl(records, is.character)
    records <- renv_records_resolve(records, latest = TRUE)
  } else {
    stopf("unexpected records format '%s'", typeof(records))
  }

  if (enrich) {
    for (i in seq_along(records)) {
      if (enrichable[[i]] && !is.null(records[[i]]))
        records[[i]] <- renv_record_enrich(records[[i]])
    }
  }

  names(records) <- enum_chr(records, function(package, record) {
    if (is.null(package) || is.na(package) || !nzchar(package))
      record[["Package"]]
    else
      package
  })

  if (is.list(lockfile))
    return(renv_lockfile_modify(lockfile, records))

  if (!file.exists(lockfile)) {
    fmt <- "no lockfile exists at path %s"
    stopf(fmt, renv_path_pretty(lockfile))
  }

  old <- renv_lockfile_read(lockfile)
  new <- renv_lockfile_modify(old, records)

  local({
    renv_scope_options(renv.verbose = FALSE)
    renv_lockfile_write(new, lockfile)
  })

  n <- length(records)
  fmt <- "- Updated %s in %s."
  writef(fmt, nplural("record", n), renv_path_pretty(lockfile))

  renv <- records[["renv"]]
  if (!is.null(renv) && !is.null(renv[["Version"]])) {
    renv_infrastructure_write_activate(
      project = project,
      version = renv[["Version"]]
    )
  }

  invisible(lockfile)

}

renv_record_normalize <- function(record) {

  # normalize source
  source <- record$Source %||% "unknown"
  if (source %in% c("CRAN", "P3M", "PPM", "RSPM"))
    record$Source <- "Repository"

  # drop remotes from records with a repository source
  if (renv_record_cranlike(record))
    record <- record[grep("^Remote", names(record), invert = TRUE)]

  # keep only specific records for comparison
  remotes <- grep("^Remote", names(record), value = TRUE)
  keep <- c("Package", "Version", "Source", remotes)
  record <- record[intersect(names(record), keep)]

  # return normalized record
  record

}

renv_record_tag <- function(record, type, url, name) {

  attr(record, "url")  <- url
  attr(record, "type") <- type
  attr(record, "name") <- name

  record

}

renv_record_tagged <- function(record) {
  attrs <- attributes(record)
  all(c("url", "type") %in% names(attrs))
}

# abstracted out in case we want to use a different sigil in the future,
# like `_`, `<NA>`, or something else
renv_record_placeholder <- function() {
  "*"
}

renv_record_cranlike <- function(record) {
  type <- record[["RemoteType"]]
  is.null(type) || tolower(type) %in% c("cran", "repository", "standard")
}

# enrich a resolved record with DESCRIPTION-level fields so the record
# matches what snapshot() would write. delegates the actual DESCRIPTION
# fetch to renv_graph_description(), which reuses the same infrastructure
# the restore graph uses (available.packages metadata + crandb fallback,
# Git host APIs, etc.). enrichment never reads the local library: we
# always trust the remote description. Hash is not computed.
renv_record_enrich <- function(record) {

  source <- renv_record_source(record, normalize = TRUE)

  enrichable <- c(
    "repository", "bioconductor",
    "github", "gitlab", "bitbucket", "git"
  )

  if (!source %in% enrichable)
    return(record)

  memoize(
    key   = renv_record_enrich_key(record, source = source),
    value = renv_record_enrich_impl(record),
    scope = "record-enrich"
  )

}

renv_record_enrich_key <- function(record, source = NULL) {

  source <- source %||% renv_record_source(record, normalize = TRUE)

  # include every field that contributes to source identity. plain git
  # remotes (Source = "git") have no RemoteSha, so distinct URLs would
  # otherwise alias to the same cache slot.
  fields <- c(
    "Package", "Version", "Source", "Repository",
    "RemoteType", "RemoteHost", "RemoteUrl", "RemoteUsername",
    "RemoteRepo", "RemoteRef", "RemoteSha", "RemoteSubdir"
  )
  parts <- map_chr(fields, function(field) record[[field]] %||% "")

  # for repository / bioconductor sources, the description fetcher reads
  # from options('repos'); include that in the key so the cache invalidates
  # when repos change mid-session. version-pinned specs in particular have
  # no Repository field, so without this the key collapses across repos.
  if (source %in% c("repository", "bioconductor")) {
    repos <- getOption("repos")
    parts <- c(
      parts,
      paste(names(repos) %||% "", repos, sep = "=", collapse = ",")
    )
  }

  paste(parts, collapse = "|")

}

renv_record_enrich_impl <- function(record) {

  package <- record[["Package"]]
  if (is.null(package) || !nzchar(package))
    stopf("cannot enrich record: missing 'Package' field")

  # delegate to the graph description fetcher; it handles every source
  # type we recognize and errors loudly when the source is unreachable
  desc <- renv_graph_description(record)

  # carry forward fields from the resolved record (Source, Repository,
  # Remote*) so they aren't lost if the description-fetcher didn't set them
  for (field in names(record))
    if (is.null(desc[[field]]))
      desc[[field]] <- record[[field]]

  # apply the v2 snapshot transform so output matches what snapshot() writes
  enriched <- renv_snapshot_description_impl_v2(desc, path = NULL)

  # prefer the named form of Repository (e.g. "CRAN") that snapshot() writes
  # when reading an installed DESCRIPTION. the description fetcher leaves
  # Repository as the contrib URL from available.packages(); use the input
  # record's value if set, else the Name stamped onto the entry by
  # renv_available_packages_entry().
  enriched[["Source"]] <- record[["Source"]] %||% enriched[["Source"]]
  enriched[["Repository"]] <-
    record[["Repository"]] %||%
    desc[["Name"]] %||%
    enriched[["Repository"]]

  # Name is a remnant of the available.packages entry, not a lockfile field
  enriched[["Name"]] <- NULL

  # Hash is not computed for enriched records; see ?record
  enriched[["Hash"]] <- NULL

  enriched

}
