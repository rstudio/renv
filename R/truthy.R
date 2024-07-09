
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

  # coerce symbols
  if (is.symbol(value))
    value <- as.character(value)

  # check for non-character values
  if (!is.character(value))
    return(as.logical(value[[1L]]))

  # check for known truthy / falsy values
  if (value[[1L]] %in% c("TRUE", "True", "true", "T", "1"))
    TRUE
  else if (value[[1L]] %in% c("FALSE", "False", "false", "F", "0"))
    FALSE
  else
    default

}
