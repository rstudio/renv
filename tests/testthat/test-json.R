
context("JSON")

test_that("sample JSON strings can be read", {

  expect_identical(
    renv_json_read(text = '[true, false, null]'),
    quote(list(TRUE, FALSE, NA))
  )

  expect_identical(
    renv_json_read(text = '{"{}": "::"}'),
    quote(list("{}" = "::"))
  )

  expect_identical(
    renv_json_read(text = '[{"a": 1.0}, {"b": -1e5}]'),
    quote(list(list(a = 1.0), list(b = -1E5)))
  )

})

test_that("we can parse a GitHub remotes specification", {
  skip_on_cran()
  skip_on_travis()
  skip_if_offline()
  data <- renv_remotes_parse_github("rstudio/renv")
  expect_true(data$Source == "GitHub")
  expect_true(data$RemoteUsername == "rstudio")
  expect_true(data$RemoteRepo == "renv")
})

test_that("we can parse a GitHub remotes specification with 'wininet'", {
  skip_on_cran()
  skip_if_not(renv_platform_windows())

  renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "wininet")
  data <- renv_remotes_parse_github("rstudio/renv")
  expect_true(data$Source == "GitHub")
  expect_true(data$RemoteUsername == "rstudio")
  expect_true(data$RemoteRepo == "renv")


})
