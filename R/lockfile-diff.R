
renv_lockfile_diff <- function(old, new, compare = NULL) {

  compare <- compare %||% function(lhs, rhs) {
    list(before = lhs, after = rhs)
  }

  # ensure both lists have the same names, inserting missing
  # entries for those without any value
  nms <- union(names(old), names(new)) %||% character()
  if (length(nms)) {

    nms <- sort(nms)
    old[renv_vector_diff(nms, names(old))] <- list(NULL)
    new[renv_vector_diff(nms, names(new))] <- list(NULL)

    old <- old[nms]
    new <- new[nms]

  }

  # check for differences
  diffs <- mapply(
    renv_lockfile_diff_impl, old, new,
    MoreArgs = list(compare = compare),
    SIMPLIFY = FALSE
  )

  # drop NULL entries
  reject(diffs, empty)

}

renv_lockfile_diff_impl <- function(lhs, rhs, compare) {
  case(
    is.list(lhs) && empty(rhs)   ~ renv_lockfile_diff(lhs, list(), compare),
    empty(lhs) && is.list(rhs)   ~ renv_lockfile_diff(list(), rhs, compare),
    is.list(lhs) && is.list(rhs) ~ renv_lockfile_diff(lhs, rhs, compare),
    !identical(c(lhs), c(rhs))   ~ compare(lhs, rhs),
    NULL
  )
}

renv_lockfile_diff_record <- function(before, after) {

  # first, compare on version / record existence
  type <- case(
    is.null(before) ~ "install",
    is.null(after)  ~ "remove",
    before$Version < after$Version ~ "upgrade",
    before$Version > after$Version ~ "downgrade"
  )

  if (!is.null(type))
    return(type)

  # check for a crossgrade -- where the package version is the same,
  # but details about the package's remotes have changed
  if (!setequal(renv_record_names(before), renv_record_names(after)))
    return("crossgrade")

  nm <- union(renv_record_names(before), renv_record_names(after))
  if (!identical(before[nm], after[nm]))
    return("crossgrade")

  NULL

}

renv_lockfile_diff_packages <- function(old, new) {

  old <- renv_records(old)
  new <- renv_records(new)

  packages <- named(union(names(old), names(new)))
  actions <- lapply(packages, function(package) {
    before <- old[[package]]; after <- new[[package]]
    renv_lockfile_diff_record(before, after)
  })

  Filter(Negate(is.null), actions)

}

renv_lockfile_override <- function(lockfile) {
  records <- renv_records(lockfile)
  overrides <- renv_records_override(records)
  renv_records(lockfile) <- overrides
  lockfile
}
