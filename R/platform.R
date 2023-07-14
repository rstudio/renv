
the$sysinfo <- NULL

renv_platform_init <- function() {
  the$sysinfo <- Sys.info()
}

renv_platform_unix <- function() {
  .Platform$OS.type == "unix"
}

renv_platform_windows <- function() {
  .Platform$OS.type == "windows"
}

renv_platform_macos <- function() {
  the$sysinfo[["sysname"]] == "Darwin"
}

renv_platform_linux <- function() {
  the$sysinfo[["sysname"]] == "Linux"
}

renv_platform_solaris <- function() {
  the$sysinfo[["sysname"]] == "SunOS"
}

renv_platform_wsl <- function() {

  pv <- "/proc/version"
  if (!file.exists(pv))
    return(FALSE)

  renv_scope_options(warn = -1L)
  contents <- catch(readLines(pv, warn = FALSE))
  if (inherits(contents, "error"))
    return(FALSE)

  any(grepl("(?:Microsoft|WSL)", contents, ignore.case = TRUE))

}

renv_platform_prefix <- function() {
  renv_bootstrap_platform_prefix()
}

renv_platform_os <- function() {
  renv_bootstrap_platform_os()
}

renv_platform_machine <- function() {
  the$sysinfo[["machine"]]
}
