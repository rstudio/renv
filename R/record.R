
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
#' @example examples/examples-record.R
#' @export
record <- function(records,
                   lockfile = NULL,
                   project  = NULL)
{
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  lockfile <- lockfile %||% renv_lockfile_path(project)

  records <- case(
    is.character(records) ~ lapply(records, renv_remotes_resolve, latest = TRUE),
    is.list(records)      ~ renv_records_resolve(records, latest = TRUE),
    ~ stopf("unexpected records format '%s'", typeof(records))
  )

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
  is.null(type) || type %in% c("cran", "standard")
}
