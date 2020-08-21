
renv_patch_init <- function() {
  renv_patch_tar()
}

renv_patch_tar <- function() {

  # read value of TAR
  tar <- Sys.getenv("TAR", unset = "")

  # on Windows, if TAR is unset, then force the usage
  # of R's internal tar implementation. this is done to
  # avoid issues where e.g. versions of tar which do not
  # understand Windows paths are on the PATH
  #
  # https://github.com/rstudio/renv/issues/521
  if (renv_platform_windows() && !nzchar(tar)) {
    Sys.setenv(TAR = "internal")
    return(TRUE)
  }

  # otherwise, allow empty / internal tars
  if (tar %in% c("", "internal"))
    return(TRUE)

  # the user (or R itself) has set the TAR environment variable
  # validate that it exists (resolve from PATH)
  resolved <- Sys.which(tar)
  if (nzchar(resolved) && file.exists(resolved))
    return(TRUE)

  # TAR appears to be set but invalid; override it
  # and warn the user
  newtar <- Sys.which("tar") %""% "internal"
  Sys.setenv(TAR = newtar)

  # report to the user
  fmt <- "requested TAR '%s' does not exist; using '%s' instead"
  warningf(fmt, tar, newtar)

}
