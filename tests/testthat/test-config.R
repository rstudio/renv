
context("Config")

test_that("config variables read from appropriate scope", {

  local({
    renv_scope_options(renv.config.test = "hello")
    expect_equal(renv_config_get("test"), "hello")
  })

  local({
    Sys.setenv(RENV_CONFIG_TEST = "hello")
    expect_equal(renv_config_get("test"), "hello")
  })

})

test_that("invalid configuration options are reported", {

  local({
    renv_scope_options(renv.config.test = "apple")
    expect_warning(renv_config_get("test", type = "integer[1]"))
  })

})

test_that("values are coerced as appropriate", {

  local({
    renv_scope_options(renv.config.test = 1)
    expect_identical(renv_config_get("test", type = "numeric[1]"), 1)
    expect_identical(renv_config_get("test", type = "integer[1]"), 1L)
    expect_identical(renv_config_get("test", type = "logical[1]"), TRUE)
  })

  local({
    renv_scope_options(renv.config.test = 0)
    expect_identical(renv_config_get("test", type = "numeric[1]"), 0)
    expect_identical(renv_config_get("test", type = "integer[1]"), 0L)
    expect_identical(renv_config_get("test", type = "logical[1]"), FALSE)
  })

})

test_that("functions are returned as-is for '*' types", {

  local({
    helper <- function() {}
    renv_scope_options(renv.config.helper = helper)
    value <- renv_config_get("helper", type = "*")
    expect_identical(value, helper)
  })

})

test_that("we can query options without warnings", {

  local({
    renv_scope_envvars(RENV_CONFIG_EXTERNAL_LIBRARIES = "/tmp")
    expect_identical(config$external.libraries(), "/tmp")
  })

})

test_that("invalid configuration options trigger a warning", {

  local({
    renv_scope_options(renv.config.connect.timeout = "oops")
    expect_warning(config$connect.timeout())
  })

})

test_that("we can query the default configuration values without issue", {

  for (key in names(config))
    config[[key]]()

})
