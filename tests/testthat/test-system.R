
test_that("system errors are reported as expected", {
  skip_on_cran()
  skip_on_os("windows")

  cnd <- catch(
    renv_system_exec(
      command = R(),
      args = c("--vanilla", "-s", "-e", renv_shell_quote("stop('barf')")),
      quiet = FALSE
    )
  )

  expect_s3_class(cnd, "error")
  expect_true(!is.null(cnd$meta))

})
