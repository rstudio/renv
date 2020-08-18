
renv_regexps_package_name <- function() {
  paste0("^", .standard_regexps()$valid_package_name, "$")
}

renv_regexps_package_version <- function() {
  paste0("^", .standard_regexps()$valid_package_version, "$")
}

