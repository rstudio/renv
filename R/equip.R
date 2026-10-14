
#' Install a build toolchain
#'
#' Equip your system with the tools required to build \R packages from
#' source. This is useful when a package recorded in the lockfile is not
#' available as a binary for your version of \R, as can happen when
#' restoring an older project.
#'
#' On Windows, `equip()` downloads and runs the installer for the version of
#' [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching the
#' running version of \R. The installer may request administrator
#' privileges. (For \R versions older than 4.0.0, `equip()` instead installs
#' the external libraries once used when building packages on Windows.)
#'
#' On macOS, `equip()` ensures the Xcode command line tools are installed.
#'
#' @return This function is normally called for its side effects.
#' @export
#' @examples
#' \dontrun{
#'
#' # install a build toolchain, if one is not already available
#' renv::equip()
#'
#' }
equip <- function() {

  renv_scope_error_handler()

  case(
    renv_platform_windows() ~ renv_equip_windows(),
    renv_platform_macos()   ~ renv_equip_macos(),
    renv_platform_linux()   ~ renv_equip_linux()
  )

  invisible(NULL)

}

renv_equip_windows <- function() {

  # Rtools 4.0 and newer bundle the libraries once provided by extsoft
  if (getRversion() < "4.0.0")
    return(invisible(renv_extsoft_install() && renv_extsoft_use()))

  invisible(renv_equip_windows_rtools())

}

renv_equip_windows_rtools <- function() {

  spec <- renv_rtools_find()
  if (!is.null(spec)) {
    fmt <- "- Rtools %s is already installed at %s."
    writef(fmt, spec$version, renv_path_pretty(spec$root))
    return(TRUE)
  }

  if (renv_rtools_path_available()) {
    writef("- A build toolchain is already available on the PATH.")
    return(TRUE)
  }

  series <- renv_rtools_required(getRversion(), renv_rtools_series_available())
  url <- renv_rtools_installer_url(series)

  if (interactive()) {

    bulletin(
      sprintf("Rtools %s will be downloaded and installed from:", series),
      url,
      "The installer may request administrator privileges."
    )

    cancel_if(!proceed())

  }

  destfile <- file.path(tempdir(), basename(url))
  download(url, destfile = destfile)
  renv_equip_windows_rtools_run(destfile)

  # the installer is run via 'start', so check for the installation itself
  # rather than relying on the exit status
  spec <- renv_rtools_find()
  if (is.null(spec)) {

    bulletin(
      "renv was unable to verify that Rtools was installed. You can run the installer manually:",
      renv_path_pretty(destfile),
      "Use the default installation location so that R can find it."
    )

    return(FALSE)

  }

  fmt <- "- Rtools %s was installed at %s."
  writef(fmt, spec$version, renv_path_pretty(spec$root))
  TRUE

}

renv_equip_windows_rtools_run <- function(installer) {

  # the installer requests elevation, which CreateProcess() (and so system2())
  # refuses; launch it through the shell instead so that Windows can prompt.
  # note that 'start' treats its first quoted argument as a window title, so
  # an empty title ("") is required for the installer path to be run
  args <- c(
    "/c", "start", "\"\"", "/wait",
    shQuote(installer),
    "/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART"
  )

  printf("- Running the Rtools installer ... ")
  status <- suppressWarnings(system2("cmd", args, stdout = FALSE, stderr = FALSE))
  if (identical(status, 0L))
    writef("Done!")
  else
    writef("FAILED [exit code %s]", status)

  invisible(status)

}

renv_equip_linux <- function() {
  stopf("renv::equip() not yet implemented for Linux")
}
