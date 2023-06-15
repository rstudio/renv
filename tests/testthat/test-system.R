
test_that("system errors are reported as expected", {
  skip_on_cran()
  skip_on_os("windows")

  expect_snapshot(

    . <- renv_system_exec(
      command = R(),
      args = c("--vanilla", "-s", "-e", renv_shell_quote("stop('barf')")),
      quiet = FALSE
    ),

    error = TRUE

  )

})
