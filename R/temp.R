
renv_tempfile_path <- function(pattern = "renv-",
                               tmpdir = renv_tempdir_path(),
                               fileext = "")
{
  path <- tempfile(pattern, tmpdir, fileext)
  renv_file_normalize(path, winslash = "/")
}

renv_tempdir_path_impl <- function() {
  dir <- Sys.getenv("RENV_TEMPDIR", unset = tempfile("renv-tempdir-"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  Sys.chmod(dir, mode = "0700")
  dir
}

renv_tempdir_path <- function() {
  renv_global("tempdir", renv_tempdir_path_impl())
}

