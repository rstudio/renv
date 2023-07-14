
truthy <- function(value, default) {
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
