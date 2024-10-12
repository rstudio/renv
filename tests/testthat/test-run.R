
test_that("run() can be called with arguments", {

  project <- renv_tests_scope()
  dir.create("renv", recursive = TRUE, showWarnings = FALSE)
  writeLines("# stub", con = "renv/activate.R")

  output <- tempfile("renv-output-")
  script <- renv_test_code({
    writeLines(commandArgs(trailingOnly = TRUE), con = output)
  }, list(output = output))

  args <- c("--apple", "--banana", "--cherry")

  run(
    script  = script,
    args    = args,
    project = getwd()
  )

  wait(file.exists, output)

  contents <- readLines(output)
  expect_equal(contents, args)

})
