
renv_tar_exe <- function() {

  # on unix, just use default
  if (renv_platform_unix())
    return(Sys.which("tar"))

  # on Windows, use system tar.exe if available
  root <- Sys.getenv("SystemRoot", unset = NA)
  if (is.na(root))
    root <- "C:/Windows"

  # use tar if it exists
  tarpath <- file.path(root, "System32/tar.exe")
  if (file.exists(tarpath))
    return(tarpath)

  # otherwise, give up (don't trust the arbitrary tar on PATH)
  ""

}

renv_tar_decompress <- function(tar, archive, files = NULL, exdir = ".", ...) {

  # build argument list
  args <- c(
    "xf", shQuote(archive),
    if (!identical(exdir, "."))
      c("-C", shQuote(exdir)),
    if (length(files))
      shQuote(files)
  )

  # make sure exdir exists
  ensure_directory(exdir)

  # perform decompress
  return(renv_system_exec(tar, args, action = "decompressing archive"))

}
