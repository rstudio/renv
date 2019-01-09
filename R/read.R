
renv_read_properties <- function(path, delimiter = ":", trim = TRUE) {

  contents <- readLines(path, warn = FALSE)
  matching <- grep(delimiter, contents, fixed = TRUE, value = TRUE)
  index <- regexpr(delimiter, matching, fixed = TRUE)
  index <- index[index != -1]

  keys <- substring(matching, 1, index - 1)
  vals <- substring(matching, index + 1)

  if (trim) {
    keys <- gsub("^\\s*|\\s*$", "", keys)
    vals <- gsub("^\\s*|\\s*$", "", vals)
  }

  names(vals) <- keys
  as.list(vals)

}
