
renv_regexps_package_name <- function() {
  paste0("^", .standard_regexps()$valid_package_name, "$")
}

renv_regexps_package_version <- function() {
  paste0("^", .standard_regexps()$valid_package_version, "$")
}

renv_regexps_escape <- function(regexp) {
  pattern <- "([\\-\\[\\]\\{\\}\\(\\)\\*\\+\\?\\.\\,\\\\\\^\\$\\|\\#\\s])"
  gsub(pattern, "\\\\\\1", regexp, perl = TRUE)
}

renv_regexps_join <- function(regexps, capture = TRUE) {
  fmt <- if (capture) "(%s)" else "(?:%s)"
  sprintf(fmt, paste(regexps, collapse = "|"))
}
