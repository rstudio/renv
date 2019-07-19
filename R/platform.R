
renv_platform_unix <- function() {
  .Platform$OS.type == "unix"
}

renv_platform_windows <- function() {
  renv_global("platform", Sys.info()[["sysname"]]) == "Windows"
}

renv_platform_macos <- function() {
  renv_global("platform", Sys.info()[["sysname"]]) == "Darwin"
}

renv_platform_linux <- function() {
  renv_global("platform", Sys.info()[["sysname"]]) == "Linux"
}
