
context("Config")

test_that("config variables read from appropriate scope", {

  local({
    renv_scope_options(renv.config.test = "hello")
    expect_equal(renv_config("test"), "hello")
  })

  local({
    Sys.setenv(RENV_CONFIG_TEST = "hello")
    expect_equal(renv_config("test"), "hello")
  })


})
