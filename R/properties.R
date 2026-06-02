
renv_properties_read <- function(path = NULL,
                                 text = NULL,
                                 delimiter = ":",
                                 dequote = TRUE,
                                 trim = TRUE)
{
  renv_scope_options(warn = -1L)

  # read file
  contents <- paste(text %||% readLines(path, warn = FALSE), collapse = "\n")

  # NOTE: we use 'useBytes = TRUE' for the perl regexps below to avoid forcing
  # PCRE into UTF mode, which can fail when R is linked against a PCRE library
  # compiled without UTF support. the patterns are pure ASCII and operate on
  # ASCII structure (newlines, comment markers), so byte-wise matching is safe;
  # we just preserve the original encoding afterwards. see also renv_dcf_read().
  encoding <- Encoding(contents)

  # split on newlines; allow spaces to continue a value
  parts <- strsplit(contents, "\\n(?=\\S)", perl = TRUE, useBytes = TRUE)[[1L]]
  Encoding(parts) <- encoding

  # remove comments and blank lines
  parts <- grep("^\\s*(?:#|$)", parts, perl = TRUE, value = TRUE, invert = TRUE, useBytes = TRUE)

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
    vals <- gsub("\n\\s*", " ", trimws(vals), perl = TRUE, useBytes = TRUE)
    Encoding(vals) <- encoding
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
