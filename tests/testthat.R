
library(testthat)
library(renv)

run <- function() {

  # disable locking in this scope
  Sys.setenv(RENV_CONFIG_LOCKING_ENABLED = FALSE)

  # check whether we can run tests here
  if (!renv:::renv_tests_supported()) {
    message("renv does not support running tests on this platform.")
    return(FALSE)
  }

  # try to initialize for testing
  status <- tryCatch(
    renv:::renv_tests_init(),
    error = identity
  )

  if (inherits(status, "error")) {
    message("renv encountered one or more problems while initializing tests.")
    message(status$message)
    return(FALSE)
  }

  # run test diagnostics
  status <- tryCatch(
    renv:::renv_tests_diagnostics(),
    error = identity
  )

  if (inherits(status, "error")) {
    message("renv encountered one or more problems while printing diagnostics.")
    message(status$message)
    return(FALSE)
  }

  # run the tests
  reporter <- renv:::renv_tests_reporter()
  test_check("renv", reporter = reporter)

}

run()

