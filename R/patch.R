
renv_patch_init <- function() {
  renv_patch_tar()
}

renv_patch_tar <- function() {

  # allow empty / internal tar
  tar <- Sys.getenv("TAR", unset = "")
  if (tar %in% c("", "internal"))
    return(TRUE)

  # validate that tar exists (allow it to be resolved on PATH)
  if (nzchar(Sys.which(tar)))
    return(TRUE)

  # TAR appears to be invalid; override it
  newtar <- Sys.which("tar")
  Sys.setenv(TAR = newtar)

  # report to the user
  fmt <- "requested TAR '%s' does not exist; using '%s' instead"
  warningf(fmt, tar, newtar)

}
