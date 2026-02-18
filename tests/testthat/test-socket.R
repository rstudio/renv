
test_that("renv_socket_accept times out when no client connects", {

  skip_on_cran()

  server <- tryCatch(renv_socket_server(), error = skip)
  defer(close(server$socket))

  # a short timeout should produce an error, not block indefinitely
  expect_error(
    renv_socket_accept(server$socket, open = "rb", timeout = 1),
    "timed out"
  )

})

test_that("renv_graph_install_accept detects crashed workers via timeout", {

  skip_on_cran()

  server <- tryCatch(renv_socket_server(), error = skip)
  defer(close(server$socket))

  port <- server$port
  now <- Sys.time()

  # simulate 3 workers; only "pkgA" and "pkgB" will connect back
  active <- list(
    pkgA = list(package = "pkgA", start = now),
    pkgB = list(package = "pkgB", start = now),
    pkgC = list(package = "pkgC", start = now)
  )

  # launch two child processes that report success
  for (pkg in c("pkgA", "pkgB")) {
    script <- renv_test_code({
      conn <- socketConnection(
        host = "127.0.0.1", port = port,
        open = "wb", blocking = TRUE, timeout = 10
      )
      serialize(list(package = pkg, exitcode = 0L, output = "ok"), conn)
      close(conn)
    }, list(port = port, pkg = pkg))

    system2(
      command = R(),
      args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      stdout  = FALSE,
      stderr  = FALSE,
      wait    = FALSE
    )
  }

  # collect results using the same accept loop pattern as renv_graph_install;
  # pkgC never connects, so accept will return NULL on timeout
  results <- list()
  collected <- character()

  for (i in seq_along(active)) {
    result <- renv_graph_install_accept(server$socket, active, timeout = 3)
    if (is.null(result))
      break
    results[[result$package]] <- result
    collected <- c(collected, result$package)
  }

  # mark uncollected workers as failed (mirrors the real install loop)
  for (pkg in setdiff(names(active), collected)) {
    results[[pkg]] <- list(
      success = FALSE,
      output  = "worker process failed to report results"
    )
  }

  # pkgA and pkgB should have succeeded
  expect_true(results$pkgA$success)
  expect_true(results$pkgB$success)

  # pkgC should be marked as failed
  expect_false(results$pkgC$success)
  expect_equal(results$pkgC$output, "worker process failed to report results")

})

test_that("we can communicate with a large number of child processes", {

  skip_on_cran()
  skip_on_ci()
  n <- 20L

  server <- tryCatch(renv_socket_server(), error = skip)
  defer(close(server$socket))

  port <- server$port
  script <- renv_test_code({
    renv:::summon()
    conn <- renv_socket_connect(port, open = "wb")
    defer(close(conn))
    serialize(Sys.getpid(), connection = conn)
  }, list(port = port))

  for (i in seq_len(n)) {
    system2(
      command = R(),
      args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      stdout = FALSE,
      stderr = FALSE,
      wait   = FALSE
    )
  }

  stk <- stack()
  for (i in seq_len(n)) local({
    conn <- renv_socket_accept(server$socket, open = "rb", timeout = 10)
    defer(close(conn))
    stk$push(unserialize(conn))
  })

  expect_length(stk$data(), n)

})
