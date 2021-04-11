
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

renv_version_equal <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) == 0
}

renv_version_match <- function(versions, request) {

  nrequest <- unclass(numeric_version(request))[[1L]]
  for (i in rev(seq_along(nrequest))) {

    matches <- which(map_lgl(versions, function(version) {
      renv_version_equal(version, request, n = i)
    }))

    if (!length(matches))
      next

    sorted <- matches[sort(names(matches), decreasing = TRUE)]
    return(names(sorted)[[1L]])

  }

  versions[[1L]]

}
