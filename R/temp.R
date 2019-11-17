
renv_tempfile <- function(pattern = "renv-",
                          tmpdir = renv_tempdir(),
                          fileext = "")
{
  path <- tempfile(pattern, tmpdir, fileext)
  norm <- renv_file_normalize(path, winslash = "/")
  defer(unlink(norm, recursive = TRUE, force = TRUE), envir = parent.frame())
  norm
}

renv_tempdir_impl <- function() {
  dir <- Sys.getenv("RENV_TEMPDIR", unset = tempfile("renv-tempdir-"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  Sys.chmod(dir, mode = "0700")
  dir
}

renv_tempdir <- function() {
  renv_global("tempdir", renv_tempdir_impl())
}

