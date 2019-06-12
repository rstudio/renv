
renv_patch_init <- function() {
  renv_patch_tar()
}

renv_patch_tar <- function() {

  tar <- Sys.getenv("TAR", unset = "")
  if (tar %in% c("", "internal"))
    return(TRUE)

  if (nzchar(Sys.which(tar)))
    return(TRUE)

  fmt <- "invalid TAR environment variable detected ('%s' does not exist)"
  warningf(fmt, tar)

  Sys.unsetenv("TAR")

}
