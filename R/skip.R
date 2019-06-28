
skip_if_no_python <- function(python = NULL) {

  python <- python %||% "python3"
  key <- paste("tests", python, "installed", sep = ".")
  installed <- renv_global(key, {
    python <- Sys.which(python)
    nzchar(python)
  })

  if (installed)
    return(python)

  testthat::skip("python is not installed")

}

skip_if_no_virtualenv <- function(python = NULL) {

  skip_if_no_python(python)

  key <- paste("tests", python, "virtualenv.installed", sep = ".")
  installed <- renv_global(key, {
    command <- paste(shQuote(python), "-m virtualenv --version")
    status <- system(command, ignore.stdout = TRUE, ignore.stderr = TRUE)
    status == 0L
  })

  if (!installed)
    testthat::skip("virtualenv module not installed")

}
