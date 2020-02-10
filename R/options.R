
renv_options_set <- function(key, value) {
  data <- list(value)
  names(data) <- key
  do.call(base::options, data)
}

renv_options_override <- function(scope, key, default = NULL) {

  if (is.null(key))
    return(default)

  value <-
    getOption(paste(scope, key, sep = ".")) %||%
    getOption(scope)[[key]] %||%
    default

  value

}
