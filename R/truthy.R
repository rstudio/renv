
truthy <- function(value, default = FALSE) {

  # https://github.com/rstudio/renv/issues/1558
  if (is.call(value)) {
    value <- tryCatch(renv_dependencies_eval(value), error = identity)
    if (inherits(value, "error"))
      return(default)
  }

  if (length(value) == 0)
    default
  else if (is.character(value))
    value %in% c("TRUE", "True", "true", "T", "1")
  else if (is.symbol(value))
    as.character(value) %in% c("TRUE", "True", "true", "T", "1")
  else if (is.na(value))
    default
  else
    as.logical(value)
}
