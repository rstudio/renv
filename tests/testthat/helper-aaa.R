
teardown_env <- function() {

  if (testthat::is_testing())
    return(testthat::teardown_env())

  globalenv()

}

`_renv_tests_repopath` <- renv_scope_tempfile("renv-repos-", scope = teardown_env())
renv_tests_repopath <- function() {
  `_renv_tests_repopath`
}
