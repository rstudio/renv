
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

renv_platform_wsl <- function() {

  pv <- "/proc/version"
  if (!file.exists(pv))
    return(FALSE)

  contents <- catch(readLines(pv, warn = FALSE))
  if (inherits(contents, "error"))
    return(FALSE)

  any(grepl("(?:Microsoft|WSL)", contents, ignore.case = TRUE))

}
