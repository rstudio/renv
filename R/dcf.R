
# similar to base::read.dcf(), but:
# - allows for whitespace between fields
# - allows for non-indented field continuations
# - always keeps whitespace
renv_dcf_read <- function(file, text = NULL, ...) {

  # read the file
  contents <- text %||% renv_file_read(file)

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
  as.data.frame(
    properties,
    optional = TRUE,
    stringsAsFactors = FALSE
  )

}

renv_dcf_write <- function(x, file = "") {
  keep.white <- c("Description", "Authors@R", "Author", "Built", "Packaged")
  write.dcf(as.list(x), file = file, indent = 4L, width = 80L, keep.white = keep.white)
}
