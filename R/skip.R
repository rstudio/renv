
skip_if_no_python <- function(python = NULL) {

  python <- python %||% "python3"
  key <- paste("tests", python, "installed", sep = ".")
  installed <- renv_global(key, {
    python <- Sys.which(python)
    nzchar(python)
  })

  if (installed)
    return(TRUE)

  testthat::skip("python is not installed")

}

skip_if_no_virtualenv <- function(python = NULL) {

  skip_if_no_python(python)

  key <- paste("tests", python, "virtualenv.installed", sep = ".")
  installed <- renv_global(key, {
    version <- renv_python_version(python)
    module <- if (numeric_version(version) >= "3.2") "venv" else "virtualenv"
    renv_python_module_available(python, module)
  })

  if (!installed)
    testthat::skip("virtualenv module not installed")

  TRUE

}

skip_if_no_miniconda <- function(python) {

  skip_if_no_python(python)
  testthat::skip_if_not_installed("reticulate", "1.13.0.9002")

  # gymnastics for CRAN checks before next release of reticulate
  if (requireNamespace("reticulate", quietly = TRUE)) {
    reticulate <- asNamespace("reticulate")
    if (is.function(reticulate$miniconda_path)) {
      path <- reticulate$miniconda_path()
      if (!file.exists(path))
        testthat::skip("miniconda is not installed")
    }
  }

  TRUE

}

skip_sometimes <- function(freq = 0.80) {
  threshold <- sample.int(100L, size = 1L)
  testthat::skip_if(freq * 100 >= threshold)
}
