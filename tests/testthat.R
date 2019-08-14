library(testthat)
library(renv)

renv:::renv_tests_init()
test_check("renv")
