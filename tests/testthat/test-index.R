
scope <- renv_id_generate()

counter <- (function() {

  .count <- 0L

  list(
    increment = function() .count <<- .count + 1L,
    get = function() .count,
    reset = function() .count <<- 0L
  )

})()

test_that("an index value is computed only once", {

  counter$reset()
  key <- renv_id_generate()

  index(
    scope = scope,
    key   = key,
    value = counter$increment()
  )

  expect_equal(counter$get(), 1L)

  index(
    scope = scope,
    key   = key,
    value = counter$increment()
  )

  expect_equal(counter$get(), 1L)

})

test_that("a timeout of 0 forces index to be re-computed", {

  counter$reset()
  key <- renv_id_generate()

  index(
    scope = scope,
    key   = key,
    value = counter$increment()
  )

  expect_equal(counter$get(), 1L)

  index(
    scope = scope,
    key   = key,
    value = counter$increment(),
    limit = 0L
  )

  expect_equal(counter$get(), 2L)

})

test_that("other processes can use the index", {

  key <- renv_id_generate()

  index(
    scope = scope,
    key   = key,
    value = TRUE
  )

  script <- renv_test_code({
    indexed <- renv:::index(scope, key, value)
    writeLines(as.character(indexed))
  }, list(scope = scope, key = key, value = FALSE))

  output <- local({
    renv_scope_envvars(RENV_PATHS_ROOT = renv_paths_root())
    renv_system_exec(
      command = R(),
      args    = c("--vanilla", "-s", "-f", shQuote(script)),
      action  = "testing renv index",
      quiet   = FALSE
    )
  })

  expect_equal(output, "TRUE")

})

test_that("a corrupt index is cleared and remade", {

  counter$reset()
  key <- renv_id_generate()

  index(
    scope = scope,
    key   = key,
    value = counter$increment()
  )

  path <- renv_paths_index(scope, "index.json")
  expect_true(file.exists(path))
  expect_equal(counter$get(), 1L)

  writeLines("BARF!", con = path)
  contents <- readLines(path)
  expect_true(identical(contents, "BARF!"))

  index(
    scope = scope,
    key   = key,
    value = counter$increment()
  )

  contents <- readLines(path)
  expect_false(identical(contents, "BARF!"))
  expect_equal(counter$get(), 2L)

})

test_that("the available packages index is updated and cleaned", {

  scope <- "available-packages"

  path <- renv_scope_tempfile("renv-index-")
  ensure_directory(path)
  renv_scope_envvars(RENV_PATHS_INDEX = path)

  renv_tests_scope()

  # request available packages
  db <- available_packages(type = "source")

  # check that an index entry was created
  idx <- index(scope)
  expect_length(idx, 1L)

  # check that we have two files in the index path
  root <- renv_paths_index(scope)
  files <- list.files(root)
  expect_length(files, 2L)

})
