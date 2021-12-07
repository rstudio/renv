
context("History")

test_that("history() on an example git repository works", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  renv_tests_scope()
  snapshot()

  system(
    "git init --quiet && git add -A && git commit --quiet -m 'initial commit'",
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )

  hist <- history()

  expect_true(nrow(hist) == 1L)
  expect_equal(hist$subject, "initial commit")

})



