
# perform setup
renv_tests_scope_setup(envir = testthat::teardown_env())

# verify that RENV_PATHS_ROOT has been set appropriately, and bail if not
local({
  rootdir <- normalizePath(dirname(renv_paths_root()), winslash = "/", mustWork = FALSE)
  tempdir <- normalizePath(tempdir(), winslash = "/")

  if (rootdir != tempdir) {
    stopf(
      paste0(
        "`renv_paths_root()` ('%s') not set to temporary directory ('%s')\n",
        "* running the tests may mutate system renv state"
      ),
      rootdir,
      tempdir
    )
  }
})

