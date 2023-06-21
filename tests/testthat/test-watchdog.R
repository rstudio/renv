
test_that("the watchdog process acquires and releases locks as expected", {

  skip_on_cran()
  skip_if(getRversion() < "4.0.0")

  script <- renv_test_code({

    renv:::summon()

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
  skip_on_ci()

  # start a socket server
  server <- tryCatch(renv_socket_server(), error = skip)
  defer(close(server$socket))

  # acquire lock in a background process
  path <- tempfile("renv-lock-")
  expect_false(file.exists(path))

  script <- renv_test_code({

    renv:::summon()

    # acquire lock in child process
    renv_scope_lock(path)

    # let parent know we have the lock
    conn <- renv_socket_connect(port, open = "wb")
    defer(close(conn))
    serialize(Sys.getpid(), connection = conn)

    # commit seppuku
    command <- paste("kill -TERM", Sys.getpid())
    system(command)

  }, list(path = path, port = server$port))

  renv_scope_envvars(RENV_WATCHDOG_ENABLED = "TRUE")
  system2(
    command = R(),
    args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    stdout = FALSE,
    stderr = FALSE,
    wait = FALSE
  )

  # get PID of child process
  pid <- local({
    conn <- renv_socket_accept(server$socket, open = "rb")
    defer(close(conn))
    unserialize(conn)
  })

  # wait for watchdog to try and remove the process
  clock <- timer()
  wait_until(function() { !file.exists(path) || clock$elapsed() > 3 })

  # check that the file no longer exists
  expect_false(file.exists(path))

})
