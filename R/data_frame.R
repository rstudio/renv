
data_frame <- function(...) {
  as_data_frame(list(...))
}

as_data_frame <- function(data) {

  # split matrices into columns
  if (is.matrix(data)) {
    result <- vector("list", ncol(data))
    names(result) <- colnames(data)
    dimnames(data) <- NULL
    for (i in seq_len(ncol(data)))
      result[[i]] <- data[, i]
    data <- result
  }

  # convert other objects to lists
  if (!is.list(data))
    data <- as.list(data)

  # recycle columns
  n <- lengths(data, use.names = FALSE)
  nrow <- max(n)

  # start recycling
  for (i in seq_along(data)) {
    if (n[[i]] == 0L) {
      length(data[[i]]) <- nrow
    } else if (n[[i]] != nrow) {
      data[[i]] <- rep.int(data[[i]], nrow / n[[i]])
    }
  }

  # set attributes
  class(data) <- "data.frame"
  attr(data, "row.names") <- .set_row_names(nrow)

  # return data
  data

}
