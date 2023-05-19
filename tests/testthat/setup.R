
renv_test_scope_setup(testthat::teardown_env())

if (dirname(renv_paths_root()) != tempdir()) {
  stop(
    "`renv_paths_root()` not set to temporary directory\n",
    "* running the tests may mutate system renv state"
  )
}
