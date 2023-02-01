
renv_options_set <- function(key, value) {
  data <- list(value)
  names(data) <- key
  do.call(base::options, data)
}

renv_options_resolve <- function(value, arguments) {

  if (is.function(value))
    return(do.call(value, arguments))

  value

}

renv_options_override <- function(scope, key, default = NULL, extra = NULL) {

  # first, check for scoped option
  value <- getOption(paste(scope, key, sep = "."))
  if (!is.null(value))
    return(renv_options_resolve(value, list(extra)))

  # next, check for unscoped option
  value <- getOption(scope)
  if (key %in% names(value))
    return(renv_options_resolve(value[[key]], list(extra)))

  # resolve option value
  if (!is.null(value))
    return(renv_options_resolve(value, list(key, extra)))

  # nothing found; use default
  default

}
