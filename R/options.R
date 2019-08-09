
renv_options_override <- function(scope, key, default = NULL) {

  value <-
    getOption(paste(scope, key, sep = ".")) %||%
    getOption(scope)[[key]] %||%
    default

  value

}
