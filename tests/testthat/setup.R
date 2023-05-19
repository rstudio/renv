
renv_test_scope_setup(testthat::teardown_env())

local({
  rootdir <- normalizePath(dirname(renv_paths_root()), winslash = "/", mustWork = FALSE)
  tempdir <- normalizePath(tempdir(), winslash = "/")

  if (rootdir != tempdir) {
    stopf(
      paste0(
        "`renv_paths_root()` ('%s') not set to temporary directory ('%s')\n",
        "* running the tests may mutate system renv state"
      ),
      root,
      temp
    )
  }
})

