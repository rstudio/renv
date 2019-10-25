library(testthat)
library(renv)

renv:::renv_tests_init()
renv:::renv_tests_diagnostics()

test_check("renv")
