
test_that(".libPaths() scoping works as expected", {

  libpaths <- .libPaths()

  local({
    dir <- renv_path_normalize(tempdir())
    renv_scope_libpaths(dir)
    expect_true(.libPaths()[1] == dir)
  })

  expect_true(.libPaths()[1] == libpaths[1])

})

test_that("options() scoping works as expected", {

  opts <- list(
    download.file.method = "curl",
    download.file.extra  = NULL
  )

  old <- options("download.file.method", "download.file.extra")
  local({
    do.call(renv_scope_options, opts)
    expect_equal(opts, options("download.file.method", "download.file.extra"))
  })

  expect_equal(old, options("download.file.method", "download.file.extra"))

})

test_that("environment variable scoping works as expected", {
  renv_scope_envvars("RENV_TEST_ENVVAR_B" = "0")

  # set and later unset a variable
  local({
    renv_scope_envvars("RENV_TEST_ENVVAR_A" = "1")
    expect_identical(Sys.getenv("RENV_TEST_ENVVAR_A"), "1")
  })
  expect_identical(Sys.getenv("RENV_TEST_ENVVAR_A", unset = NA), NA_character_)

  # override and restore variables
  local({
    renv_scope_envvars("RENV_TEST_ENVVAR_A" = "1", "RENV_TEST_ENVVAR_B" = "2")
    expect_identical(Sys.getenv("RENV_TEST_ENVVAR_A"), "1")
    expect_identical(Sys.getenv("RENV_TEST_ENVVAR_B"), "2")
  })
  expect_identical(Sys.getenv("RENV_TEST_ENVVAR_B"), "0")
  expect_identical(Sys.getenv("RENV_TEST_ENVVAR_A", unset = NA), NA_character_)

  # unset and reset variable
  local({
    renv_scope_envvars("RENV_TEST_ENVVAR_B" = NULL)
    expect_identical(Sys.getenv("RENV_TEST_ENVVAR_B", unset = NA), NA_character_)
  })
  expect_identical(Sys.getenv("RENV_TEST_ENVVAR_B"), "0")

})

test_that("nested attempts to scope libpaths are properly handled", {

  libpaths <- .libPaths()

  local({
    dir <- renv_path_normalize(tempdir())
    renv_scope_libpaths(dir)
    renv_scope_libpaths(dir)
    expect_true(.libPaths()[1] == dir)
  })

  expect_true(.libPaths()[1] == libpaths[1])

})

test_that("renv_scope_trace works", {

  count <- 0

  local({
    renv_scope_trace(what = identity, tracer = function() count <<- count + 1)
    identity(1)
  })

  identity(1)

  expect_equal(count, 1L)

})

test_that("can temporarily replace binding", {
  envir <- environment()
  x <- 10

  local({
    renv_scope_binding(envir, "x", 20)
    expect_equal(x, 20)
  })

  expect_equal(x, 10)
})

test_that("can temporarily create binding", {
  envir <- environment()

  local({
    renv_scope_binding(envir, "x", 20)
    expect_equal(x, 20)
  })

  expect_false(exists("x", inherits = FALSE))
})

test_that("can temporarily replace locked binding", {
  original <- utils::adist

  utils <- asNamespace("utils")
  local({
    renv_scope_binding(utils, "adist", 20)
    expect_equal(utils::adist, 20)
  })

  expect_equal(utils::adist, original)
})
