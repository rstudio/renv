
bind <- function(data, names = NULL, index = "Index") {

  # keep only non-empty data
  filtered <- Filter(NROW, data)
  if (!length(filtered))
    return(NULL)

  # ensure all datasets have the same column names
  # try to preserve the ordering of names if possible
  # (try to find one dataset which has all column relevant column names)
  nms <- character()
  for (i in seq_along(filtered)) {
    names(filtered[[i]]) <- names(filtered[[i]]) %||% names
    nmsi <- names(filtered[[i]])
    if (empty(setdiff(nms, nmsi)))
      nms <- nmsi
  }

  # check now if we've caught all relevant names; if we didn't,
  # just fall back to a "dumb" union
  allnms <- unique(unlist(lapply(filtered, names)))
  if (!setequal(nms, allnms))
    nms <- allnms

  # we've collected all names; now fill with NAs as necessary
  filled <- map(filtered, function(datum) {
    datum[setdiff(nms, names(datum))] <- NA
    datum[nms]
  })

  # we've collected and ordered each data.frame, now merge them
  rhs <- .mapply(c, filled, list(use.names = FALSE))
  names(rhs) <- names(filled[[1L]])

  if (is.null(names(data))) {
    names(rhs) <- names(rhs) %||% names
    return(as.data.frame(rhs, stringsAsFactors = FALSE))
  }

  if (index %in% names(rhs)) {
    fmt <- "name collision: bound list already contains column called '%s'"
    stopf(fmt, index)
  }

  lhs <- list()
  rows <- function(item) nrow(item) %||% length(item[[1]])
  lhs[[index]] <- rep.int(names(filled), times = map_dbl(filled, rows))

  cbind(
    as.data.frame(lhs, stringsAsFactors = FALSE),
    as.data.frame(rhs, stringsAsFactors = FALSE)
  )

}





