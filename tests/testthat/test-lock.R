
context("Lock")

test_that("lockfiles can be created, destroyed", {

  path <- renv_path_canonicalize(tempfile())

  callback <- renv_lock_create(path)
  expect_true(is.function(callback))
  expect_true(file.exists(path))

  idpath <- file.path(path, "id")
  expect_true(file.exists(idpath))

  id <- readLines(idpath)
  expect_equal(`_renv_locks`[[path]], id)

  renv_lock_destroy(path)
  expect_false(file.exists(path))
  expect_false(file.exists(idpath))
  expect_null(`_renv_locks`[[path]])

})

test_that("lockfile locks are removed via callback", {

  path <- renv_path_canonicalize(tempfile())

  local({
    callback <- renv_lock_create(path)
    on.exit(callback(), add = TRUE)
    expect_true(file.exists(path))
  })

  expect_false(file.exists(path))

})

test_that("nested locks are handled correctly", {

  path <- renv_path_canonicalize(tempfile())

  local({
    callback <- renv_lock_create(path)
    on.exit(callback(), add = TRUE)
    local({
      callback <- renv_lock_create(path)
      on.exit(callback(), add = TRUE)
      expect_true(file.exists(path))
    })
    expect_true(file.exists(path))
  })

  expect_false(file.exists(path))

})

test_that("multiple calls to renv_scope_lock() do the right thing", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_path_canonicalize(tempfile())

  local({

    # this entry creates the lock
    expect_false(file.exists(path))
    renv_scope_lock(path)
    expect_true(file.exists(path))

    local({

      # a lock already exists, but it's owned by us,
      # so we still treat this as a success
      expect_true(file.exists(path))
      renv_scope_lock(path)
      expect_true(file.exists(path))

    })

    # the previous renv_scope_lock should not have cleaned up the lock yet
    expect_true(file.exists(path))

  })

  # all renv_scope_lock() functions are out of scope now;
  # the lock should have been removed
  expect_false(file.exists(path))

})
