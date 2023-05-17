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
    envir = testthat::teardown_env()
  )

  needs_tz <- renv_platform_macos() && !nzchar(Sys.getenv("TZ"))
  if (needs_tz) {
    renv_scope_envvars(
      TZ = "America/Los_Angeles",
      envir = testthat::teardown_env()
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
    envir = testthat::teardown_env()
  )
}

# Force loading of packages from current .libPaths(); needed for packages
# that would otherwise loaded in a renv_tests_scope()
renv_tests_init_packages <- function() {
  requireNamespace("waldo", quietly = TRUE)
  renv_namespace_load("crayon", quietly = TRUE)

  if (!isNamespaceLoaded("pak")) {
    usr <- file.path(tempdir(), "usr-cache")
    ensure_directory(file.path(usr, "R/renv"))

    pkg <- file.path(tempdir(), "pkg-cache")
    ensure_directory(pkg)

    renv_scope_envvars(
      R_USER_CACHE_DIR = usr,
      R_PKG_CACHE_DIR  = pkg
    )

    requireNamespace("pak")
    # trigger package load in pak subprocess
    pak <- renv_namespace_load("pak")
    pak$remote(function() {})
  }
}


# cache path before working directory gets changed
renv_tests_root()

renv_tests_init_envvars()
renv_tests_init_options()
renv_tests_init_packages()
