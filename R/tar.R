
renv_tar_exe <- function(default = "") {

  # allow override
  tar <- getOption("renv.tar.exe")
  if (!is.null(tar))
    return(tar)

  # Sys.which() spawns a process, which adds up when called per package
  memoize(
    key   = paste("default", default),
    value = renv_tar_exe_impl(default)
  )

}

renv_tar_exe_impl <- function(default) {

  # on unix, just use default
  if (renv_platform_unix()) {
    tar <- Sys.which("tar")
    if (nzchar(tar))
      return(tar)
  }

  # on Windows, use system tar.exe if available
  root <- Sys.getenv("SystemRoot", unset = NA)
  if (is.na(root))
    root <- "C:/Windows"

  # use tar if it exists
  tarpath <- file.path(root, "System32/tar.exe")
  if (file.exists(tarpath))
    return(tarpath)

  # otherwise, give up (don't trust the arbitrary tar on PATH)
  default

}

renv_tar_decompress <- function(tar, archive, files = NULL, exdir = ".", ...) {

  # build argument list
  args <- c(
    "xf", renv_shell_path(archive),
    if (!identical(exdir, "."))
      c("-C", renv_shell_path(exdir)),
    if (length(files))
      renv_shell_path(files)
  )

  # make sure exdir exists
  ensure_directory(exdir)

  # perform decompress
  return(renv_system_exec(tar, args, action = "decompressing archive"))

}

# the tar utils::untar() uses by default in this session: TAR if set,
# otherwise (on Windows) whatever 'tar.exe' resolves to, otherwise R's
# internal implementation. this is R's choice, not renv's; renv's own
# calls use renv_tar_exe() or the internal tar. reported by diagnostics()
#
# note that R resolves executables on Windows via the Win32 SearchPath()
# API, which checks the system directory before the PATH, so a bare
# 'tar.exe' is the system tar whenever one exists; the PATH only matters
# on Windows builds without one
renv_tar_r <- function() {

  tar <- Sys.getenv("TAR", unset = "")
  if (nzchar(tar))
    return(tar)

  if (renv_platform_windows()) {
    tar <- Sys.which("tar.exe")
    if (nzchar(tar))
      return(renv_path_normalize(tar))
  }

  "internal"

}

# the Windows system tar (bsdtar), if available
renv_tar_system <- function() {

  if (!renv_platform_windows())
    return("")

  root <- Sys.getenv("SystemRoot", unset = "C:/Windows")
  path <- file.path(root, "System32/tar.exe")
  if (file.exists(path)) renv_path_normalize(path) else ""

}
