
renv_version_compare <- function(lhs, rhs, n = NULL) {

  # retrieve versions as integer vector
  lhs <- unlist(unclass(numeric_version(lhs)))
  rhs <- unlist(unclass(numeric_version(rhs)))

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

renv_version_parts <- function(version, n) {

  # split version into parts
  parts <- unclass(as.numeric_version(version))[[1L]]

  # extend parts to size of n
  diff <- max(n) - length(parts)
  if (diff > 0)
    parts <- c(parts, rep.int(0L, diff))

  # retrieve possibly-extended parts
  parts[1:n]

}

renv_version_maj_min <- function(version) {
  parts <- renv_version_parts(version, 2L)
  paste(parts, collapse = ".")
}

renv_version_length <- function(version) {
  nv <- as.numeric_version(version)
  length(unclass(nv)[[1L]])
}

# parse version strings into an integer matrix with one row per
# version and 'ncols' columns, zero-padded for shorter versions.
# replaced by C implementation when ext is enabled.
renv_version_matrix <- function(versions, ncols) {

  parts <- strsplit(versions, "[.-]", perl = TRUE)
  lens <- lengths(parts, use.names = FALSE)

  flat <- unlist(parts, use.names = FALSE)
  row <- rep.int(seq_along(versions), lens)
  col <- sequence(lens)

  n <- length(versions)
  mat <- matrix(0L, nrow = n, ncol = ncols)

  keep <- col <= ncols
  mat[row[keep] + (col[keep] - 1L) * n] <- as.integer(flat[keep])

  mat

}

# produce an integer ranking of version strings, suitable for use
# with order(). avoids the overhead of numeric_version() by splitting
# version strings into an integer matrix and ranking the result.
renv_version_rank <- function(versions) {

  mat <- renv_version_matrix(versions, ncols = 6L)

  # order then rank, so the result can be composed with other
  # sort keys via order(packages, renv_version_rank(versions))
  cols <- lapply(seq_len(ncol(mat)), function(i) mat[, i])
  ord <- do.call(order, cols)
  rank <- integer(length(versions))
  rank[ord] <- seq_along(ord)
  rank

}
