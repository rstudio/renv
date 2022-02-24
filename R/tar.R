
renv_tar_exe <- function() {

  # on unix, just use default
  if (renv_platform_unix())
    return(Sys.which("tar"))

  # otherwise, try to use system-default tar
  root <- Sys.getenv("SystemRoot", unset = NA)
  if (is.na(root))
    root <- "C:/Windows"

  # use tar if it exists
  tarpath <- file.path(root, "System32/tar.exe")
  if (file.exists(tarpath))
    return(tarpath)

  # otherwise, just try to use whatever is on the PATH
  Sys.which("tar.exe")

}
