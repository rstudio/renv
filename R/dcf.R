
# similar to base::read.dcf(), but:
# - allows for whitespace between fields
# - allows for non-indented field continuations
# - always keeps whitespace
renv_dcf_read <- function(file, text = NULL, ...) {

  # read the file as binary first to get encoding
  contents <- text %||% renv_dcf_read_impl(file, ...)

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

renv_dcf_read_impl <- function(file, ...) {

  # first, read the file as bytes to get encoding
  contents <- readBin(
    con  = file,
    what = "raw",
    n    = renv_file_size(file)
  )

  # try to find encoding -- if none is declared, assume native encoding?
  start <- grepRaw("(?:^|\n)Encoding:", contents)
  if (empty(start))
    return(rawToChar(contents))

  # try to find the end of the encoding field
  end   <- grepRaw("(?:\r?\n|$)", contents, offset = start + 1L)
  field <- rawToChar(contents[start:end])

  # parse it
  properties <- renv_properties_read(text = field)

  # now convert from this encoding to UTF-8
  iconv(
    x    = list(contents),
    from = properties$Encoding,
    to   = "UTF-8"
  )

}

renv_dcf_write <- function(x, file = "") {
  keep.white <- c("Description", "Authors@R", "Author", "Built", "Packaged")
  write.dcf(as.list(x), file = file, indent = 4L, width = 80L, keep.white = keep.white)
}
