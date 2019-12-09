
`_renv_sysinfo` <- NULL

renv_platform_init <- function() {
  `_renv_sysinfo` <<- Sys.info()
}

renv_platform_unix <- function() {
  .Platform$OS.type == "unix"
}

renv_platform_windows <- function() {
  .Platform$OS.type == "windows"
}

renv_platform_macos <- function() {
  `_renv_sysinfo`[["sysname"]] == "Darwin"
}

renv_platform_linux <- function() {
  `_renv_sysinfo`[["sysname"]] == "Linux"
}

renv_platform_solaris <- function() {
  `_renv_sysinfo`[["sysname"]] == "SunOS"
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
