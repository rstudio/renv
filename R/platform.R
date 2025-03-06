
the$distro   <- NULL
the$os       <- NULL
the$platform <- NULL
the$prefix   <- NULL
the$sysinfo  <- NULL

renv_platform_init <- function() {

  the$sysinfo <- as.list(Sys.info())

  the$platform <- if (file.exists("/etc/os-release")) {
    renv_properties_read(
      path      = "/etc/os-release",
      delimiter = "=",
      dequote   = TRUE,
      trim      = TRUE
    )
  }

  the$os <- tolower(the$sysinfo$sysname)

  # NOTE: This is chosen to be compatible with the distribution field
  # used within r-system-requirements.
  if (the$os == "linux") {
    aliases <- list(rhel = "redhat")
    the$distro <- alias(the$platform$ID, aliases)
  }

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
  (the$prefix <- the$prefix %||% renv_bootstrap_platform_prefix())
}

renv_platform_os <- function() {
  renv_bootstrap_platform_os()
}

renv_platform_machine <- function() {
  the$sysinfo[["machine"]]
}
