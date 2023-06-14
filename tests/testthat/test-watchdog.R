
test_that("the watchdog process acquires and releases locks as expected", {

  skip_on_cran()
  skip_if(!renv_watchdog_enabled())

  path <- tempfile()
  renv_watchdog_notify("LockAcquired", list(path = path))
  locks <- renv_watchdog_request("ListLocks")
  expect_equal(locks, path)

  renv_watchdog_notify("LockReleased", list(path = path))
  locks <- renv_watchdog_request("ListLocks")
  expect_equal(locks, character())

})

test_that("the watchdog process releases locks from killed processes", {

  skip_on_cran()
  skip_if(!renv_watchdog_enabled())

  # acquire lock in a background process
  path <- tempfile("renv-lock-")
  expect_false(file.exists(path))

  script <- renv_test_code({
    renv:::renv_scope_lock(path)
    Sys.sleep(1)
    system(paste("kill -TERM", Sys.getpid()))
  }, list(path = path))

  system2(
    command = R(),
    args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
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

test_that("multiple renv processes successfully acquire, release locks", {

  skip_on_cran()
  skip_if(!renv_watchdog_enabled())

  # initialize server
  server <- renv_socket_server()
  defer(close(server$socket))

  # initialize state
  n <- 200
  shared <- renv_scope_tempfile("renv-file-")
  lockfile <- tempfile("renv-lock-")

  # initialize shared file
  writeLines("0", con = shared)

  # generate runner script
  script <- renv_test_code(

    code = {

      # sleep a small, random amount of time
      Sys.sleep(runif(1, max = 0.1))

      # update shared file with lock acquired
      lock <- renv:::renv_lock_acquire(lockfile)
      number <- as.integer(readLines(shared))
      writeLines(as.character(number + 1L), con = shared)
      renv:::renv_lock_release(lockfile)

      # notify parent
      conn <- socketConnection(port = port, open = "w+b", blocking = TRUE)
      serialize(number, connection = conn)

      # we're done
      invisible()

    },

    data = list(
      lockfile = lockfile,
      shared = shared,
      port = server$port
    )

  )


  # create a bunch of processes that try to update the shared file
  for (i in 1:n) {
    system2(
      command = R(),
      args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      stdout = FALSE,
      stderr = FALSE,
      wait = FALSE
    )
  }

  # wait for all the processes to communicate
  responses <- stack()
  for (i in 1:n) {
    conn <- renv_socket_accept(server$socket, open = "r+b", timeout = 10)
    data <- unserialize(conn)
    close(conn)
    responses$push(data)
  }

  # check that the count is correct
  contents <- readLines(shared)
  expect_equal(contents, as.character(n))

  # check that each process saw a unique value
  numbers <- unlist(responses$data())
  expect_equal(sort(numbers), 0:199)

})
