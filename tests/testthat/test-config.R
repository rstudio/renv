
test_that("config variables read from appropriate scope", {

  local({
    renv_scope_options(renv.config.test = "hello")
    expect_equal(renv_config_get("test"), "hello")
  })

  local({
    renv_scope_envvars(RENV_CONFIG_TEST = "hello")
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

  expect_true(TRUE)

})

test_that("multiple library paths can be set in RENV_CONFIG_EXTERNAL_LIBRARIES", {

  renv_scope_envvars(RENV_CONFIG_EXTERNAL_LIBRARIES = "/a:/b:/c")
  libpaths <- config$external.libraries()
  expect_equal(libpaths, c("/a", "/b", "/c"))

})

test_that("cache symlinks are disabled if the cache and project library lie in different volumes", {

  skip_on_cran()
  skip_if(renv_platform_unix())

  project <- renv_tests_scope()

  renv_scope_envvars(RENV_PATHS_CACHE = "//network/drive")
  expect_false(renv_cache_config_symlinks(project = project))

  projlib <- renv_paths_library(project = project)
  renv_scope_envvars(RENV_PATHS_CACHE = dirname(projlib))
  expect_true(renv_cache_config_symlinks(project = project))


})

test_that("RENV_CONFIG_EXTERNAL_LIBRARIES is decoded appropriately", {

  envname <- "RENV_CONFIG_EXTERNAL_LIBRARIES"

  expect_equal(
    renv_config_decode_envvar(envname, "/apple"),
    "/apple"
  )

  expect_equal(
    renv_config_decode_envvar(envname, "C:/apple"),
    "C:/apple"
  )

  expect_equal(
    renv_config_decode_envvar(envname, "C:/apple:C:/banana"),
    c("C:/apple", "C:/banana")
  )

  expect_equal(
    renv_config_decode_envvar(envname, "C:/apple;C:/banana"),
    c("C:/apple", "C:/banana")
  )

  expect_equal(
    renv_config_decode_envvar(envname, "C:/apple,C:/banana"),
    c("C:/apple", "C:/banana")
  )

  expect_equal(
    renv_config_decode_envvar(envname, "/apple:/banana"),
    c("/apple", "/banana")
  )

  expect_equal(
    renv_config_decode_envvar(envname, "/apple;/banana"),
    c("/apple", "/banana")
  )

  expect_equal(
    renv_config_decode_envvar(envname, "/apple,/banana"),
    c("/apple", "/banana")
  )

})
