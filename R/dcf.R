
# similar to base::read.dcf(), but:
# - allows for whitespace between fields
# - allows for non-indented field continuations
# - always keeps whitespace
renv_dcf_read <- function(file, text = NULL, ...) {

  # read file
  contents <- text %||% renv_dcf_read_impl(file, ...)

  # split on newlines
  parts <- strsplit(contents, "\\r?\\n(?=\\S)", perl = TRUE)[[1L]]

  # remove embedded newlines
  parts <- gsub("\\r?\\n\\s*", " ", parts, perl = TRUE)

  # split into key / value pairs
  index <- regexpr(":", parts, fixed = TRUE)
  keys <- substring(parts, 1L, index - 1L)
  vals <- substring(parts, index + 1L)

  # trim whitespace
  vals <- trimws(vals)

  # return early if everything looks fine
  ok <- nzchar(keys)
  if (all(ok)) {
    storage.mode(vals) <- "list"
    names(vals) <- keys
    return(vals)
  }

  # otherwise, fix up bad continuations
  starts <- which(ok)
  ends <- c(tail(starts - 1L, n = -1L), length(keys))
  vals <- .mapply(
    function(start, end) paste(vals[start:end], collapse = " "),
    list(starts, ends),
    NULL
  )

  # set up names
  names(vals) <- keys[ok]

  # done
  vals

}

renv_dcf_read_impl_encoding <- function(bytes) {

  # try to find encoding -- if none is declared, assume native encoding?
  start <- 0L
  while (TRUE) {

    # find 'Encoding'
    start <- grepRaw("Encoding:", bytes, fixed = TRUE, offset = start + 1L)
    if (length(start) == 0L)
      return(NULL)

    # check for preceding newline, or start of file
    if (start == 1L || bytes[[start - 1L]] == 0x0A) {
      start <- start + 9L
      break
    }

  }

  # find the end of the encoding field
  end <- grepRaw("\\r?\\n", bytes, offset = start + 1L)
  if (length(end) == 0L)
    end <- length(bytes)

  # pull it out
  field <- rawToChar(bytes[start:end])
  trimws(field)

}

renv_dcf_read_impl <- function(file, ...) {

  # suppress warnings in this scope
  renv_scope_options(warn = -1L)

  # first, read the file as bytes to get encoding
  # use a guess for the file size to avoid expensive lookup, but fallback
  # if necessary
  bytes <- readBin(file, what = "raw", n = 8192L)
  if (length(bytes) == 8192L) {
    n <- renv_file_size(file)
    bytes <- readBin(con = file, what = "raw", n = n)
  }

  # try to guess the encoding
  encoding <- renv_dcf_read_impl_encoding(bytes)

  # try a bunch of candidate encodings
  encodings <- c(encoding, "UTF-8", "latin1", "")
  for (encoding in unique(encodings)) {
    result <- iconv(list(bytes), from = encoding, to = "UTF-8")
    if (!is.na(result))
      return(result)
  }

  # all else fails, just pretend it's in the native encoding
  rawToChar(bytes)

}

renv_dcf_write <- function(x, file = "") {

  # NOTE: Older versions of write.dcf() will coerce the value into a data.frame
  # without setting 'optional = TRUE'; make sure we do this ourselves first
  value <- as_data_frame(x)

  keep.white <- c("Description", "Authors@R", "Author", "Built", "Packaged")
  result <- write.dcf(value, file = file, indent = 4L, width = 80L, keep.white = keep.white)

  renv_filebacked_invalidate(file)

  invisible(result)

}
