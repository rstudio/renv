
# similar to base::read.dcf(), but:
# - allows for whitespace between fields
# - allows for non-indented field continuations
# - always keeps whitespace
renv_dcf_read <- function(file, text = NULL, ...) {

  # read the file as binary first to get encoding
  contents <- text %||% renv_dcf_read_impl(file, ...)

  # normalize newlines
  contents <- gsub("\r\n", "\n", contents, fixed = TRUE)

  # look for tags
  pattern <- "(?:^|\n)[^\\s][^:\n]*:"
  matches <- gregexpr(pattern, contents, perl = TRUE)[[1L]]

  # compute substring indices
  starts <- matches
  ends   <- c(tail(matches, n = -1L), nchar(contents))
  parts <- substring(contents, starts, ends)

  # read as property list
  properties <- renv_properties_read(text = parts, dequote = FALSE)

  # set encoding if necessary
  if (identical(properties$Encoding, "UTF-8"))
    properties[] <- lapply(properties, renv_encoding_mark, "UTF-8")

  # return as data.frame
  as.list(properties)

}

renv_dcf_read_impl_encoding <- function(contents) {

  # try to find encoding -- if none is declared, assume native encoding?
  start <- grepRaw("(?:^|\n)Encoding:", contents)
  if (empty(start))
    return(NULL)

  # try to find the end of the encoding field
  end   <- grepRaw("(?:\r?\n|$)", contents, offset = start + 1L)
  field <- rawToChar(contents[start:end])

  # parse it
  properties <- renv_properties_read(text = field)
  properties[["Encoding"]]

}

renv_dcf_read_impl <- function(file, ...) {

  # suppress warnings in this scope
  renv_scope_options(warn = -1L)

  # first, read the file as bytes to get encoding
  n <- renv_file_size(file)
  contents <- readBin(con = file, what = "raw", n = n)

  # try to guess the encoding
  encoding <- tryCatch(
    renv_dcf_read_impl_encoding(contents),
    error = function(e) NULL
  )

  # try a bunch of candidate encodings
  encodings <- c(encoding, "UTF-8", "latin1", "")
  for (encoding in unique(encodings)) {
    result <- iconv(list(contents), from = encoding, to = "UTF-8")
    if (!is.na(result))
      return(result)
  }

  # all else fails, just pretend it's in the native encoding
  rawToChar(contents)

}

renv_dcf_write <- function(x, file = "") {
  keep.white <- c("Description", "Authors@R", "Author", "Built", "Packaged")
  write.dcf(as.list(x), file = file, indent = 4L, width = 80L, keep.white = keep.white)
}
