
renv_type_check <- function(value, type) {

  # quietly convert NAs to requested type
  if (is.null(value) || is.na(value))
    return(convert(value, type))

  # if the value already matches the expected type, return success
  if (inherits(value, type))
    return(value)

  # create error object
  fmt <- "parameter '%s' is not of expected type '%s'"
  msg <- sprintf(fmt, deparse(substitute(value)), type)
  error <- simpleError(msg, sys.call(sys.parent()))

  # report error
  stop(error)

}

renv_type_unexpected <- function(value) {
  fmt <- "parameter '%s' has unexpected type '%s'"
  msg <- sprintf(fmt, deparse(substitute(value)), typeof(value))
  error <- simpleError(msg, sys.call(sys.parent()))
  stop(error)
}
