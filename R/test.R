
test <- function() {

  # ensure tools loaded
  devtools::load_all()

  # run tests
  testthat::test_dir(
    path     = renv_tests_root(),
    reporter = renv_tests_reporter()
  )

}
