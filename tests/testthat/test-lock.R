
test_that("locks can be acquired, released", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(renv_scope_tempfile())

  renv_lock_acquire(path)
  expect_true(file.exists(path))

  renv_lock_release(path)
  expect_false(file.exists(path))

})

test_that("scoped locks are released appropriately", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(renv_scope_tempfile())

  local({
    renv_scope_lock(path)
    expect_true(file.exists(path))
  })

  expect_false(file.exists(path))

})

test_that("we can recursively acquire locks", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  path <- renv_lock_path(renv_scope_tempfile())

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
  path <- renv_lock_path(renv_scope_tempfile())

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
  path <- renv_lock_path(renv_scope_tempfile())

  code <- expr({
    renv_lock_acquire(!!path)
    stopifnot(file.exists(!!path))
  })

  args <- c("--vanilla", "-s", "-e", shQuote(stringify(code)))
  status <- suppressWarnings(
    system2(R(), args, stdout = FALSE, stderr = FALSE, timeout = 1L)
  )

  expect_equal(status, 1L)
  expect_false(file.exists(path))

})

test_that("we can refresh locks", {

  # create a file
  path <- renv_scope_tempfile("renv-lock-")
  file.create(path)

  # get current info
  old <- file.info(path, extra_cols = FALSE)

  # wait a bit
  Sys.sleep(2)

  # refresh the 'lock'
  renv_lock_refresh(path)
  new <- file.info(path, extra_cols = FALSE)

  # check for updated time
  expect_gt(new$mtime, old$mtime)

})


test_that("old locks are considered 'orphaned'", {

  renv_scope_options(renv.config.locking.enabled = TRUE)
  renv_scope_envvars(RENV_WATCHDOG_ENABLED = "FALSE")

  path <- renv_lock_path(renv_scope_tempfile())

  renv_scope_options(renv.lock.timeout = -1L)
  renv_lock_acquire(path)

  expect_true(renv_lock_orphaned(path))
  expect_true(file.exists(path))

  script <- renv_test_code({
    options(renv.config.locking.enabled = TRUE)
    options(renv.lock.timeout = -1L)
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

test_that("multiple renv processes successfully acquire, release locks", {

  skip_on_cran()
  skip_if(getRversion() < "4.0.0")
  skip_on_os("windows")
  skip_on_ci()

  renv_scope_options(renv.config.locking.enabled = TRUE)
  renv_scope_envvars(RENV_WATCHDOG_ENABLED = "FALSE")

  # initialize server
  server <- tryCatch(renv_socket_server(), error = skip)
  defer(close(server$socket))

  # initialize state
  n <- 100
  start <- tempfile("renv-start-")
  lockfile <- renv_lock_path(tempfile("renv-lock-"))

  # initialize shared file
  shared <- renv_scope_tempfile("renv-file-")
  writeLines("0", con = shared)

  # generate runner script
  script <- renv_test_code(

    code = {

      renv:::summon()

      # wait for start file
      wait_until(file.exists, start)

      # helper function
      increment <- function() {
        renv_lock_acquire(lockfile)
        stopifnot(file.exists(lockfile))
        number <- as.integer(readLines(shared))
        writeLines(as.character(number + 1L), con = shared)
        renv_lock_release(lockfile)
        number
      }

      # update shared file with lock acquired
      number <- catch(increment())
      if (inherits(number, "error"))
        number <- -1L

      # notify parent
      conn <- renv_socket_connect(port = port, open = "wb")
      defer(close(conn))
      serialize(number, connection = conn)

      # we're done
      invisible()

    },

    data = list(
      start = start,
      lockfile = lockfile,
      shared = shared,
      port = server$port
    )

  )

  # create start file
  file.create(start)

  # create a bunch of processes that try to update the shared file
  for (i in 1:n) {
    system2(
      command = R(),
      args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      wait = FALSE
    )
  }

  # wait for all the processes to communicate
  responses <- stack()
  for (i in 1:n) local({
    conn <- renv_socket_accept(server$socket, open = "rb", timeout = 60)
    defer(close(conn))
    responses$push(unserialize(conn))
  })

  # check that the count is correct
  contents <- readLines(shared)
  expect_equal(contents, as.character(n))

  # check that each process saw a unique value
  numbers <- unlist(responses$data())
  expect_equal(sort(numbers), 0:(n - 1))

})
