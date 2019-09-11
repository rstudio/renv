
# tools for interacting with the R package records encoded
# within a lockfile
renv_records <- function(records) {
  if (inherits(records, "renv_lockfile"))
    return(records$Packages)
  records
}

`renv_records<-` <- function(x, value) {
  x$Packages <- value
  x
}

renv_records_select <- function(records, actions, action) {
  records <- renv_records(records)
  records[names(actions[actions == action])]
}

renv_records_sort <- function(records) {
  renv_scope_locale("LC_COLLATE", "C")
  records[order(names(records))]
}

renv_records_override <- function(records) {
  enumerate(records, renv_options_override, scope = "renv.records")
}

renv_records_repos_latest <- function(package) {

  types <- renv_package_pkgtypes()

  # iterate through available packages reported by all repositories
  # and look for a matching entry
  entries <- bapply(types, function(type) {

    entry <- catch(renv_available_packages_entry(package, type))
    if (inherits(entry, "error"))
      return(NULL)

    entry[c("Package", "Version", "Repository", "Type", "Name")]

  })

  if (!is.data.frame(entries)) {
    fmt <- "package '%s' is not available on CRAN"
    stopf(fmt, package)
  }

  # since multiple entries could match, take the newest version by default
  # TODO: could also allow older binary version here
  idx <- with(entries, order(Version, factor(Type, c("source", "binary"))))
  entry <- entries[tail(idx, n = 1), ]

  record <- list(
    Package    = package,
    Version    = entry$Version,
    Source     = "Repository",
    Repository = entry$Name
  )

  # annotate record with extra information
  attr(record, "type") <- entry$Type
  attr(record, "url")  <- entry$Repository

  record

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
  source <- record$Source %||% "unknown"
  if (source == "unknown")
    return(FALSE)

  # record is ok
  TRUE

}

renv_record_validate <- function(record, quiet = FALSE) {

  # TODO
  TRUE

}
