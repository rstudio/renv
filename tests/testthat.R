
library(testthat)
library(renv)

renv:::renv_tests_init()
renv:::renv_tests_diagnostics()

reporter <- renv:::renv_tests_reporter()
test_check("renv", reporter = reporter)
