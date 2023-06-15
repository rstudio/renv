
test_that("the watchdog process acquires and releases locks as expected", {

  skip_on_cran()
  skip_if(getRversion() < "4.0.0")

  script <- renv_test_code({

    renv:::renv()

    path <- tempfile()
    renv_watchdog_notify("LockAcquired", list(path = path))
    locks <- renv_watchdog_request("ListLocks")
    assert(path %in% locks)

    renv_watchdog_notify("LockReleased", list(path = path))
    locks <- renv_watchdog_request("ListLocks")
    assert(!path %in% locks)

  })

  renv_scope_envvars(RENV_WATCHDOG_ENABLED = "TRUE")
  output <- expect_no_error(
    system2(
      command = R(),
      args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      stdout = FALSE,
      stderr = FALSE,
      timeout = 10
    )
  )

})

test_that("the watchdog process releases locks from killed processes", {

  skip_on_cran()
  skip_if(getRversion() < "4.0.0")

  # acquire lock in a background process
  path <- tempfile("renv-lock-")
  expect_false(file.exists(path))

  script <- renv_test_code({
    renv:::renv()
    renv_scope_lock(path)
    command <- paste("kill -TERM", Sys.getpid())
    system(command)
  }, list(path = path))

  renv_scope_envvars(RENV_WATCHDOG_ENABLED = "TRUE")
  system2(
    command = R(),
    args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    stdout = FALSE,
    stderr = FALSE,
    wait = FALSE
  )

  # wait for the file to exist
  wait_until(file.exists, path)
  expect_true(file.exists(path))

  # give the watchdog some time to run
  clock <- timer(units = "secs")
  wait_until(function() {
    Sys.sleep(0.1)
    !file.exists(path) || clock$elapsed() > 3
  })

  # check that the file no longer exists
  expect_false(file.exists(path))

})
