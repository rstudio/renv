
renv_properties_read <- function(path = NULL,
                                 text = NULL,
                                 delimiter = ":",
                                 dequote = TRUE,
                                 trim = TRUE)
{
  renv_scope_options(warn = -1L)

  # read file
  contents <- paste(text %||% readLines(path, warn = FALSE), collapse = "\n")

  # split on newlines; allow spaces to continue a value
  parts <- strsplit(contents, "\\n(?=\\S)", perl = TRUE)[[1L]]

  # remove comments and blank lines
  parts <- grep("^\\s*(?:#|$)", parts, perl = TRUE, value = TRUE, invert = TRUE)

  # split into key / value pairs
  index <- regexpr(delimiter, parts, fixed = TRUE)

  # if we couldn't match a delimiter, treat the whole thing as a key
  missed <- index == -1
  index[missed] <- nchar(parts)[missed] + 1L

  # perform the subsetting
  keys <- substring(parts, 1L, index - 1L)
  vals <- substring(parts, index + 1L)

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
  storage.mode(vals) <- "list"
  names(vals) <- keys

  vals

}
