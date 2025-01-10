
teardown_env <- function() {
  if (testthat::is_testing())
    testthat::teardown_env()
  else
    globalenv()
}

the$tests_repopath <- renv_scope_tempfile("renv-repos-", scope = teardown_env())

renv_tests_repopath <- function() {
  the$tests_repopath
}
