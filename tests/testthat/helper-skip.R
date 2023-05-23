skip_if_no_github_auth <- function() {
  skip_if(is.na(Sys.getenv("GITHUB_PAT", unset = NA)), "GITHUB_PAT not set")
}

skip_if_no_python <- function(python = NULL) {

  python <- python %||% "python3"
  key <- paste("tests", python, "installed", sep = ".")
  installed <- global(key, {
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
  installed <- global(key, {
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

skip_if_local <- function() {
  ci <- Sys.getenv("CI", unset = NA)
  testthat::skip_if(is.na(ci), "Running tests locally")
}

skip_on_windows <- function() {
  testthat::skip_on_os("windows")
}
