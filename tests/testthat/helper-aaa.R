
teardown_env <- function() {
  if (interactive()) globalenv() else testthat::teardown_env()
}

`_renv_tests_repopath` <- renv_scope_tempfile("renv-repos-", envir = teardown_env())

renv_tests_repopath <- function() {
  `_renv_tests_repopath`
}
