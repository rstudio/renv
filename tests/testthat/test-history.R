
context("History")

test_that("history() on an example git repository works", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  renv_tests_scope()
  snapshot()

  # try initializing a simple git repository
  renv_system_exec("git", c("init", "--quiet"), action = "git init")
  renv_system_exec("git", c("add", "-A"), action = "git add files")
  renv_system_exec("git", c("commit", "-m", shQuote("initial commit")))

  # retrieve history
  hist <- history()

  # check that it worked
  expect_true(nrow(hist) == 1L)
  expect_equal(hist$subject, "initial commit")

})



