
test_that("we can communicate with a large number of child processes", {

  n <- 20L

  server <- renv_socket_server()
  defer(close(server$socket))

  script <- renv_test_code({
    renv:::renv()
    conn <- renv_socket_connect(port, open = "wb")
    defer(close(conn))
    serialize(Sys.getpid(), connection = conn)
  }, list(port = server$port))

  for (i in seq_len(n)) {
    system2(
      command = R(),
      args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      stdout = FALSE,
      stderr = FALSE,
      wait = FALSE
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
