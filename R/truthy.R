
renv_truthy_impl <- function(value, values, default) {

  if (is.call(value)) {
    value <- catch(renv_dependencies_eval(value))
    if (inherits(value, "error"))
      return(default)
  }

  case(
    is.null(value)      ~ default,
    is.symbol(value)    ~ as.character(value) %in% values,
    is.na(value)        ~ default,
    is.logical(value)   ~ value,
    is.character(value) ~ value %in% values,
    is.numeric(value)   ~ value != 0,
    ~ tryCatch(as.logical(value), error = function(e) default)
  )

}

truthy <- function(value, default) {
  values <- c("TRUE", "True", "true", "T", "1")
  renv_truthy_impl(value, values, default)
}

falsy <- function(value, default) {
  values <- c("FALSE", "False", "false", "F", "0")
  renv_truthy_impl(value, values, default)
}
