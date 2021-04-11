
renv_version_compare <- function(lhs, rhs, n = NULL) {

  # retrieve versions as integer vector
  lhs <- unclass(numeric_version(lhs))[[1L]]
  rhs <- unclass(numeric_version(rhs))[[1L]]

  # compute number of components to compare
  n <- n %||% max(length(lhs), length(rhs))

  # pad each vector with zeroes up to the requested length
  lhs <- c(lhs, rep.int(0L, max(0L, n - length(lhs))))
  rhs <- c(rhs, rep.int(0L, max(0L, n - length(rhs))))

  # iterate through each component and compare
  for (i in seq_len(n)) {
    if (lhs[[i]] < rhs[[i]])
      return(-1L)
    else if (lhs[[i]] > rhs[[i]])
      return(+1L)
  }

  # if we got here, then all components compared equal
  0L

}

renv_version_le <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) <= 0L
}

renv_version_lt <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) <  0L
}

renv_version_eq <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) == 0L
}

renv_version_gt <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) >  0L
}

renv_version_ge <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) >= 0L
}

renv_version_match <- function(versions, request) {

  nrequest <- unclass(numeric_version(request))[[1L]]
  for (i in rev(seq_along(nrequest))) {

    matches <- which(map_lgl(versions, function(version) {
      renv_version_eq(version, request, n = i)
    }))

    if (!length(matches))
      next

    # TODO: should '3.1' match the closest match (e.g. '3.2') or
    # highest match (e.g. '3.6')?
    sorted <- matches[sort(names(matches), decreasing = TRUE)]
    return(names(sorted)[[1L]])

  }

  versions[[1L]]

}

renv_version_component <- function(version, n) {
  parts <- unclass(numeric_version(version))[[1L]]
  if (n > length(parts)) 0L else parts[[n]]
}
