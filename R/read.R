
renv_read_properties <- function(path, delimiter = ":", trim = TRUE) {

  # read lines and drop empty lines, commented values
  contents <- readLines(path, warn = FALSE)
  contents <- contents[nzchar(contents)]
  contents <- grep("^\\s*[#;]", contents, value = TRUE, invert = TRUE)

  # find the delimiter for each line
  contents <- grep(delimiter, contents, fixed = TRUE, value = TRUE)
  index <- regexpr(delimiter, contents, fixed = TRUE)
  index <- index[index != -1]

  # separate into keys, values
  keys <- substring(contents, 1, index - 1)
  vals <- substring(contents, index + 1)

  # trim whitespace when requested
  if (trim) {
    keys <- trimws(keys)
    vals <- trimws(vals)
  }

  # return as named list
  names(vals) <- keys
  as.list(vals)

}
