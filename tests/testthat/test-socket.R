
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

test_that("persistent socket connections detect results and worker death", {

  skip_on_cran()

  server <- tryCatch(renv_socket_server(), error = skip)
  defer(close(server$socket))

  port <- server$port

  # launch two workers that follow the persistent connection protocol:
  # 1. connect, 2. send package name (hello), 3. send result, 4. close
  for (pkg in c("pkgA", "pkgB")) {
    script <- renv_test_code({
      conn <- socketConnection(
        host = "127.0.0.1", port = port,
        open = "wb", blocking = TRUE, timeout = 10
      )
      serialize(pkg, conn)
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

  # launch a worker that connects and sends hello, then crashes (no result)
  script <- renv_test_code({
    conn <- socketConnection(
      host = "127.0.0.1", port = port,
      open = "wb", blocking = TRUE, timeout = 10
    )
    serialize("pkgC", conn)
    close(conn)
  }, list(port = port))

  system2(
    command = R(),
    args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    stdout  = FALSE,
    stderr  = FALSE,
    wait    = FALSE
  )

  # accept all three hello connections via socketSelect on the server socket
  conns <- list()
  for (i in 1:3) {
    socketSelect(list(server$socket), write = FALSE, timeout = 10)
    conn <- socketAccept(server$socket, open = "rb", blocking = TRUE, timeout = 10)
    pkg <- unserialize(conn)
    conns[[pkg]] <- conn
  }

  expect_equal(sort(names(conns)), c("pkgA", "pkgB", "pkgC"))

  # now use socketSelect on all three worker connections to read results
  results <- list()
  remaining <- names(conns)

  while (length(remaining) > 0L) {
    active_conns <- conns[remaining]
    flags <- socketSelect(active_conns, write = FALSE, timeout = 10)
    for (i in which(flags)) {
      pkg <- remaining[i]
      data <- tryCatch(unserialize(conns[[pkg]]), error = function(e) NULL)
      close(conns[[pkg]])
      if (is.null(data)) {
        results[[pkg]] <- list(success = FALSE, output = "worker died")
      } else {
        results[[pkg]] <- list(
          success = identical(as.integer(data$exitcode), 0L),
          output  = data$output
        )
      }
    }
    remaining <- setdiff(remaining, remaining[flags])
  }

  # pkgA and pkgB sent results
  expect_true(results$pkgA$success)
  expect_true(results$pkgB$success)

  # pkgC connected but sent no result (EOF detected)
  expect_false(results$pkgC$success)
  expect_equal(results$pkgC$output, "worker died")

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
