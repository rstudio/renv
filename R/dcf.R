
# similar to base::read.dcf(), but:
# - allows for whitespace between fields
# - allows for non-indented field continuations
# - always keeps whitespace
renv_dcf_read <- function(file, text = NULL, ...) {

  encoding <- "UTF-8"

  if(is.null(text)){
    path <- `if`(
      inherits(file, "connection"),
      unlist(summary(file))["description"],
      file
    )
    raw_contents <- readBin(path, "raw", file.size(path))
    enc_offset <- grepRaw("Encoding:", raw_contents)
    if(length(enc_offset)){
      line_length <- grepRaw("\n", tail(raw_contents, -enc_offset))
      raw_enc <- head(tail(raw_contents, -enc_offset - 9L), line_length - 10L)
      encoding <- rawToChar(raw_enc)
    }
  }

  # read the file
  contents <- text %||% renv_file_read(file, encoding = encoding)

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

renv_dcf_write <- function(x, file = "") {
  keep.white <- c("Description", "Authors@R", "Author", "Built", "Packaged")
  write.dcf(as.list(x), file = file, indent = 4L, width = 80L, keep.white = keep.white)
}
