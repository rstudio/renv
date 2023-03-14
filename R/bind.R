
bind <- function(data, names = NULL, index = "Index") {

  # keep only non-empty data
  data <- Filter(NROW, data)
  if (!length(data))
    return(NULL)

  # check for quick exit
  if (length(data) == 1L) {

    # no-name case
    if (is.null(names(data))) {
      rhs <- data[[1L]]
      names(rhs) <- names(rhs) %||% names
      return(as_data_frame(rhs))
    }

    # named case
    lhs <- list(rep.int(names(data), times = NROW(data[[1L]])))
    names(lhs) <- index
    rhs <- as.list(data[[1L]])
    return(as_data_frame(c(lhs, rhs)))

  }

  # ensure all datasets have the same column names
  # try to preserve the ordering of names if possible
  # (try to find one dataset which has all column relevant column names)
  nms <- character()
  for (i in seq_along(data)) {
    names(data[[i]]) <- names(data[[i]]) %||% names
    nmsi <- names(data[[i]])
    if (length(nmsi) > length(nms))
      nms <- nmsi
  }

  # check now if we've caught all relevant names; if we didn't,
  # just fall back to a "dumb" union
  allnms <- unique.default(unlist(lapply(data, names), use.names = FALSE))
  if (!setequal(nms, allnms))
    nms <- allnms

  # we've collected all names; now fill with NAs as necessary
  filled <- map(data, function(datum) {
    datum[setdiff(nms, names(datum))] <- NA
    datum[nms]
  })

  # we've collected and ordered each data.frame, now merge them
  rhs <- .mapply(c, filled, list(use.names = FALSE))
  names(rhs) <- names(filled[[1L]])

  if (is.null(names(data))) {
    names(rhs) <- names(rhs) %||% names
    return(as_data_frame(rhs))
  }

  if (index %in% names(rhs)) {
    fmt <- "name collision: bound list already contains column called '%s'"
    stopf(fmt, index)
  }

  lhs <- list()
  rows <- function(item) nrow(item) %||% length(item[[1L]])
  lhs[[index]] <- rep.int(names(filled), times = map_dbl(filled, rows))

  as_data_frame(c(lhs, rhs))

}





