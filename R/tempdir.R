
renv_tempdir_init <- function() {
  
  # only check on linux
  if (!renv_platform_linux())
    return()

  # allow disable via envvar if needed
  check <- Sys.getenv("RENV_TEMPDIR_NOEXEC_CHECK", unset = "TRUE")
  if (not(check))
    return()
    
  # check that scripts within the R temporary directory can be executed
  script <- tempfile("renv-script-", fileext = ".sh")
  writeLines("#!/usr/bin/env sh", con = script)
  Sys.chmod(script, mode = "0755")
  on.exit(unlink(script), add = TRUE)
  
  status <- system(script, ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (identical(status, 0L))
    return()
  
  fmt <- heredoc("
  
    The R temporary directory appears to be within a folder mounted as 'noexec'.
    Installation of R packages from sources may fail.
    See the section **Note** within `?INSTALL` for more details.
    
    tempdir(): %s
    
  ")
  
  caution(fmt, tempdir(), con = stderr())
  
}
