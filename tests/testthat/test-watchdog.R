
test_that("the watchdog process releases locks from killed processes", {

  skip_on_cran()
  skip_if(getRversion() < "4.0.0")

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
