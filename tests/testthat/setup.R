
# TODO: This is a hack to deal with interactive running of tests when
# testthat::teardown_env() hasn't been initialized for some reason
renv_tests_envir <- function() {
  tryCatch(
    testthat::teardown_env(),
    error = function(e) globalenv()
  )
}

# perform setup
renv_tests_scope_setup(envir = renv_tests_envir())

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

