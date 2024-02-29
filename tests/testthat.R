
library(tools)
library(testthat)
library(renv, warn.conflicts = FALSE)

if (!renv:::renv_tests_supported()) {
  message("* renv does not support running tests on this platform.")
  if (!interactive()) quit(status = 0L)
}

if (Sys.info()[["sysname"]] == "Linux") {
  cachedir <- R_user_dir("renv", "cache")
  dir.create(cachedir, recursive = TRUE, showWarnings = FALSE)
}

test_check("renv")
