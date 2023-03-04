
context("Lock")

test_that("locks can be acquired, released", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(tempfile())

  renv_lock_acquire(path)
  expect_true(file.exists(path))

  renv_lock_release(path)
  expect_false(file.exists(path))

})

test_that("scoped locks are released appropriately", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(tempfile())

  local({
    renv_scope_lock(path)
    expect_true(file.exists(path))
  })

  expect_false(file.exists(path))

})

test_that("we can recursively acquire locks", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(tempfile())

  local({

    renv_scope_lock(path)
    expect_true(file.exists(path))

    local({
      renv_scope_lock(path)
      expect_true(file.exists(path))
    })

    expect_true(file.exists(path))

  })

  expect_false(file.exists(path))

})

test_that("other processes cannot lock our owned locks", {

  skip_if(
    is.null(formals(system2)[["timeout"]]),
    "system2() lacks the timeout argument"
  )

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(tempfile())

  renv_lock_acquire(path)

  script <- renv_test_code(
    print(renv:::renv_lock_acquire(path)),
    list(path = path)
  )

  args <- c("--vanilla", "-s", "-f", shQuote(script))
  output <- suppressWarnings(
    system2(R(), args, stdout = FALSE, stderr = FALSE, timeout = 1L)
  )

  expect_equal(output, 124L)

})

test_that("locks are released on process exit", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(tempfile())

  code <- substitute({
    renv:::renv_lock_acquire(path)
    stopifnot(file.exists(path))
  }, list(path = path))

  args <- c("--vanilla", "-s", "-e", shQuote(stringify(code)))
  status <- suppressWarnings(
    system2(R(), args, stdout = FALSE, stderr = FALSE, timeout = 1L)
  )

  expect_equal(status, 1L)
  expect_false(file.exists(path))

})

test_that("old locks are considered 'orphaned'", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(tempfile())

  renv_scope_options(renv.lock.timeout = 0L)
  renv_lock_acquire(path)

  expect_true(renv_lock_orphaned(path))
  expect_true(file.exists(path))

  script <- renv_test_code({
    options(renv.config.locking.enabled = TRUE)
    options(renv.lock.timeout = 0L)
    stopifnot(renv:::renv_lock_acquire(path))
    stopifnot(file.exists(path))
  }, list(path = path))

  output <- renv_system_exec(
    command = R(),
    args    = c("--vanilla", "-s", "-f", shQuote(script)),
    action  = "checking for orphaned locks",
  )

  expect_false(file.exists(path))

})
