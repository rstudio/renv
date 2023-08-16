
renv_scoped_sandbox <- function(scope = parent.frame()) {
  renv_scope_options(renv.config.sandbox.enabled = TRUE, scope = scope)

  sandbox <- renv_scope_tempfile(scope = scope)
  renv_scope_envvars(RENV_PATHS_SANDBOX = sandbox, scope = scope)

  old <- list(.Library.site, .Library, .libPaths())
  defer(scope = scope, {
    unlink(sandbox, recursive = TRUE, force = TRUE)
    renv_binding_replace(base, ".Library.site", old[[1L]])
    renv_binding_replace(base, ".Library", old[[2L]])
    .libPaths(old[[3]])
  })
}

test_that("the sandbox can be activated and deactivated", {

  renv_scoped_sandbox()

  # save current library paths
  libpaths <- .libPaths()

  # after activating the sandbox, .Library should be changed
  syslib <- renv_libpaths_system()
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))

  # after deactivating the sandbox, .Library should be restored
  renv_sandbox_deactivate()
  expect_equal(syslib, .Library)

  # the library paths should be restored as well
  expect_equal(libpaths, .libPaths())

})

test_that("multiple attempts to activate sandbox are handled", {

  renv_scoped_sandbox()

  libpaths <- .libPaths()
  syslib <- renv_libpaths_system()

  # calls to renv_sandbox_activate() should be idempotent
  renv_sandbox_activate()
  renv_sandbox_activate()
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))

  # deactivate the sandbox and assert we've restored state
  renv_sandbox_deactivate()
  expect_equal(syslib, .Library)
  expect_equal(libpaths, .libPaths())

})

test_that(".Library.site isn't used even when sandbox is disabled", {

  skip_if(renv_platform_windows() || empty(.Library.site))

  renv_scope_options(renv.config.sandbox.enabled = FALSE)

  sandbox <- renv_scope_tempfile("renv-sandbox-")
  renv_scope_envvars(RENV_PATHS_SANDBOX = sandbox)

  sitelib <- setdiff(.Library.site, .Library)
  renv_sandbox_activate()
  expect_false(any(sitelib %in% .libPaths()))
  renv_sandbox_deactivate()

})

test_that("renv repairs library paths on load if sandbox is active", {

  renv_scoped_sandbox()

  libpaths <- .libPaths()
  syslib <- renv_libpaths_system()

  # initialize the sandbox
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))

  # check for sandbox marker file
  marker <- file.path(.Library, ".renv-sandbox")
  expect_true(file.exists(marker))

  # set up R_LIBS, and then run a child process that reports
  # the library paths after loading renv
  script <- renv_test_code({
    renv:::summon()
    writeLines(.libPaths())
  })

  output <- local({
    renv_scope_envvars(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
    renv_system_exec(
      command = R(),
      args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      action  = "testing sandbox"
    )
  })

  expect_equal(output, .libPaths())

  # deactivate the sandbox and assert we've restored state
  renv_sandbox_deactivate()
  expect_equal(syslib, .Library)
  expect_equal(libpaths, .libPaths())

})

test_that("multiple processes can attempt to acquire the sandbox", {

  skip_on_cran()
  skip_on_ci()

  renv_scoped_sandbox()

  # number of processes
  n <- 20

  server <- tryCatch(renv_socket_server(), error = skip)
  defer(close(server$socket))

  project <- renv_tests_scope()

  script <- renv_test_code({
    renv:::summon()
    renv_sandbox_activate(project)
    conn <- renv_socket_connect(port = port, open = "wb")
    defer(close(conn))
    writeLines(paste(Sys.getpid()), con = conn)
  }, list(project = project, file = file, port = server$port))

  for (i in seq_len(n)) {
    system2(
      command = R(),
      args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      wait = FALSE
    )
  }

  stk <- stack()
  for (i in seq_len(n)) local({
    conn <- renv_socket_accept(server$socket, open = "rb", timeout = 10)
    defer(close(conn))
    stk$push(readLines(conn))
  })

  expect_length(stk$data(), n)

})
