
truthy <- function(value, default = FALSE) {

  # https://github.com/rstudio/renv/issues/1558
  if (is.call(value)) {
    value <- tryCatch(renv_dependencies_eval(value), error = identity)
    if (inherits(value, "error"))
      return(default)
  }

  # skip empty vectors
  if (length(value) == 0L)
    return(default)

  # handle symbols
  if (is.symbol(value))
    value <- as.character(value)

  # only look at first element in vector
  value <- value[[1L]]

  # handle some non-character types up-front
  if (is.call(value))
    return(default)
  else if (is.na(value))
    return(default)
  else if (!is.character(value))
    return(as.logical(value))

  # check for known truthy / falsy values
  if (value %in% c("TRUE", "True", "true", "T", "1"))
    TRUE
  else if (value %in% c("FALSE", "False", "false", "F", "0"))
    FALSE
  else
    default

}
