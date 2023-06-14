
skip_if_no_github_auth <- function() {
  skip_if_not(renv_envvar_exists("GITHUB_PAT"), "GITHUB_PAT is not set")
}

skip_if_no_python <- function() {
  has_python <- Sys.which("python3") != "" || Sys.which("python") != ""
  skip_if_not(has_python, "python is not installed")
}

sys_python <- function() {

  for (python in c("python3", "python")) {
    python <- Sys.which(python)
    if (nzchar(python))
      return(python)
  }

  skip("python is not available")

}

skip_if_no_virtualenv <- function() {
  skip_if_no_python()
  skip_if_not(has_virtualenv(), "virtualenv module not installed")
  TRUE
}

the$has_virtualenv <- NULL
has_virtualenv <- function() {
  the$has_virtualenv <- the$has_virtualenv %||% {
    python <- sys_python()
    version <- renv_python_version(python)
    module <- if (numeric_version(version) >= "3.2") "venv" else "virtualenv"
    renv_python_module_available(python, module)
  }
}

skip_if_no_miniconda <- function(min_version = NULL) {
  skip_if_no_python()

  if (!is.null(min_version)) {
    has_version <- renv_version_ge(renv_python_version(sys_python()), min_version)
    skip_if_not(has_version, paste0("python ", min_version, " not installed"))
  }

  skip_if_not_installed("reticulate", "1.28")
  path <- reticulate::miniconda_path()
  skip_if_not(file.exists(path), "miniconda is not installed")
}

skip_if_local <- function() {
  ci <- Sys.getenv("CI", unset = NA)
  testthat::skip_if(is.na(ci), "Running tests locally")
}

skip_on_windows <- function() {
  testthat::skip_on_os("windows")
}
