
renv_properties_read <- function(path = NULL,
                                 text = NULL,
                                 delimiter = ":",
                                 dequote = TRUE,
                                 trim = TRUE)
{
  text <- text %||% readLines(path, warn = FALSE)

  # drop empty lines, commented values
  text <- text[nzchar(text)]
  text <- grep("^\\s*[#;]", text, value = TRUE, invert = TRUE)

  # find the delimiter for each line
  text <- grep(delimiter, text, fixed = TRUE, value = TRUE)
  index <- regexpr(delimiter, text, fixed = TRUE)
  index <- index[index != -1]

  # separate into keys, values
  keys <- substring(text, 1, index - 1)
  vals <- substring(text, index + nchar(delimiter))

  # trim whitespace when requested
  if (trim) {
    keys <- trimws(keys)
    vals <- trimws(vals)
  }

  # strip quotes if requested
  if (dequote) {
    keys <- dequote(keys)
    vals <- dequote(vals)
  }

  # return as named list
  names(vals) <- keys
  as.list(vals)

}
