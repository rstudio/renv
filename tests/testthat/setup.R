
`_renv_tests_envir` <- NULL

# helper function for running exit handlers registered on globalenv;
# mainly for interactive use
if (interactive()) {
  makeActiveBinding("done", function(value) {
    renv_defer_execute(envir = globalenv())
  }, env = globalenv())
}

# TODO: This is a hack to deal with interactive running of tests when
# testthat::teardown_env() hasn't been initialized for some reason
renv_tests_envir <- function() {

  `_renv_tests_envir` <<- `_renv_tests_envir` %||% {
    tryCatch(
      testthat::teardown_env(),
      error = function(e) globalenv()
    )
  }

}

renv_tests_init_envvars <- function() {
  renv_scope_envvars(
    # simulate running in R CMD check
    "_R_CHECK_PACKAGE_NAME_" = "renv",
    # disable locking in this scope
    RENV_CONFIG_LOCKING_ENABLED = FALSE,
    RENV_PATHS_ROOT = NULL,
    RENV_PATHS_LIBRARY = NULL,
    RENV_PATHS_LIBRARY_ROOT = NULL,
    RENV_PATHS_LOCAL = NULL,
    RENV_PATHS_LOCKFILE = NULL,
    RENV_PATHS_RENV = NULL,
    RENV_AUTOLOAD_ENABLED = FALSE,
    envir = renv_tests_envir()
  )

  needs_tz <- renv_platform_macos() && !nzchar(Sys.getenv("TZ"))
  if (needs_tz) {
    renv_scope_envvars(
      TZ = "America/Los_Angeles",
      envir = renv_tests_envir()
    )
  }

  envvars <- Sys.getenv()
  configvars <- grep("^RENV_CONFIG_", names(envvars), value = TRUE)
  Sys.unsetenv(configvars)
}

renv_tests_init_options <- function() {
  # find path to renv sources
  sources <- renv_file_find(getwd(), function(parent) {
    descpath <- file.path(parent, "DESCRIPTION")
    if (file.exists(descpath))
      return(parent)
  })

  renv_scope_options(
    # set it so we can find the sources
    renv.test.sources = sources,
    renv.config.user.library = FALSE,
    renv.config.sandbox.enabled = TRUE,
    restart = NULL,
    warn = 2,
    # don't perform transactional installs by default for now
    # (causes strange CI failures, especially on Windows?)
    renv.config.install.transactional = FALSE,
    # mark tests as running
    renv.tests.running = TRUE,
    envir = renv_tests_envir()
  )
}

# perform setup
renv_tests_scope_setup(renv_tests_envir())

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

