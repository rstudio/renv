
library(testthat)
library(renv)

renv:::renv_tests_init()
renv:::renv_tests_diagnostics()

result <- test_check("renv")

renv:::renv_tests_report(result)
