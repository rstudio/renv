
test_that("renv handles unexpected user agent values", {

  # https://github.com/rstudio/renv/issues/1787
  renv_scope_options(HTTPUserAgent = character())
  agent <- renv_http_useragent()
  expect_equal(agent, renv_http_useragent_default())

  renv_scope_options(HTTPUserAgent = NULL)
  agent <- renv_http_useragent()
  expect_equal(agent, renv_http_useragent_default())

})
