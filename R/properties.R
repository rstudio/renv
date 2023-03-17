
renv_properties_read <- function(path = NULL,
                                 text = NULL,
                                 delimiter = ":",
                                 dequote = TRUE,
                                 trim = TRUE)
{
  renv_scope_options(warn = -1L)
  text <- paste(text %||% readLines(path, warn = FALSE), collapse = "\n")

  # remove comments
  text <- gsub("(?:^|\n)\\s*#[^\\n]\\n", "\n", text, perl = TRUE)

  # remove blank lines
  text <- gsub("\n\\s*\n", "\n", text, perl = TRUE)

  # find the locations of fields
  fmt <- "(?:^|\n)(?!\\s)[^%1$s\n]+%1$s"
  pattern <- sprintf(fmt, delimiter)
  locations <- gregexpr(pattern, text, perl = TRUE)[[1L]]

  # pull each part out
  starts <- locations
  ends   <- c(tail(locations - 1L, n = -1L), nchar(text))

  # fix up start locations
  newlines <- substring(text, starts, starts) == "\n"
  starts[newlines] <- starts[newlines] + 1L

  # pull everything out
  text <- substring(text, starts, ends)

  # find the delimiter for each line
  index <- regexpr(delimiter, text, fixed = TRUE)

  # separate into keys, values
  keys <- substring(text, 1L, index - 1L)
  vals <- substring(text, index + nchar(delimiter))

  # trim whitespace when requested
  if (trim) {
    keys <- trimws(keys)
    vals <- gsub("\n\\s*", " ", trimws(vals), perl = TRUE)
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
