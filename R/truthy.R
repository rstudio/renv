
truthy <- function(value, default = FALSE) {

  if (!is.atomic(value) || length(value) != 1 || is.na(value))
    default
  else if (is.character(value))
    value %in% c("TRUE", "True", "true", "T", "1")
  else
    as.logical(value)
}

