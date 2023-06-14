
teardown_env <- function() {
  if (testthat::is_testing())
    testthat::teardown_env()
  else
    globalenv()
}

`_renv_tests_repopath` <- renv_scope_tempfile("renv-repos-", scope = teardown_env())
renv_tests_repopath <- function() {
  `_renv_tests_repopath`
}
