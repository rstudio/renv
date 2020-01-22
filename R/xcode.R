
renv_xcode_check <- function() {

  # allow bypass of xcode check if required
  check <- getOption("renv.xcode.check", default = TRUE)
  if (identical(check, FALSE))
    return()

  # only run on macOS
  if (!renv_platform_macos())
    return()

  # only run check once per session
  if (renv_once())
    return()

  cmd <- "/usr/bin/xcrun --find --show-sdk-path"
  status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (identical(status, 0L))
    return()

  if (identical(status, 69L)) {

    msg <- "
macOS is reporting that you have not yet agreed to the Xcode license.
You must accept the Xcode license before R packages can be installed from source.
Please run:

    sudo xcodebuild -license accept

in the Terminal to accept the Xcode license.
Set options(renv.xcode.check = FALSE) to disable this warning.
"
    warning(msg)

  }

  fmt <- "%s returned exit code %i"
  warningf(fmt, cmd, status)

}
