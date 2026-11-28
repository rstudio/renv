
skip_if_no_github_auth <- function() {
  token <- renv_bootstrap_github_token()
  skip_if(empty(token), "GITHUB_PAT is not set")
  skip_if_github_rate_limited()
}

# CI jobs share one hourly GitHub API budget per repository. Once it is spent
# every request fails until the hour is up, so tests needing the API are
# skipped while too little is left. The margin allows for the requests other
# jobs make between this check and the test's own.
skip_if_github_rate_limited <- function(limits = github_rate_limits()) {
  remaining <- limits$resources$core$remaining
  fmt <- "GitHub API rate limit is nearly exhausted (%i requests remaining)"
  skip_if(remaining < 100L, sprintf(fmt, remaining))
}

github_rate_limits <- function() {

  # requests to this endpoint are not counted against the rate limit
  url <- "https://api.github.com/rate_limit"
  destfile <- renv_scope_tempfile("renv-github-rate-limit-", fileext = ".json")

  status <- catch(
    download(url, destfile = destfile, type = "github", quiet = TRUE)
  )

  if (inherits(status, "error"))
    skip(paste("GitHub API is not available:", conditionMessage(status)))

  renv_json_read(destfile)

}

skip_if_no_bioconductor <- function() {
  skip_if_not_installed("BiocManager")
  skip_if_not(
    renv_bioconductor_supported(),
    "Bioconductor does not support this version of R"
  )
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
