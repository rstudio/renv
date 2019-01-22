
renv_platform_sysname_is <- function(name) {
  Sys.info()[["sysname"]] == name
}

renv_platform_windows <- function() {
  renv_platform_sysname_is("Windows")
}

renv_platform_macos <- function() {
  renv_platform_sysname_is("Darwin")
}

renv_platform_linux <- function() {
  renv_platform_sysname_is("Linux")
}
