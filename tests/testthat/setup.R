renv_scope_envvars(
  # simulate running in R CMD check
  "_R_CHECK_PACKAGE_NAME_" = "renv",
  # disable locking in this scope
  RENV_CONFIG_LOCKING_ENABLED = FALSE,
  envir = testthat::teardown_env()
)
