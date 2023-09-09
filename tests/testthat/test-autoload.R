
test_that("autoload() works", {
  skip_on_cran()

  # TODO: Failed on Windows CI, but works locally?
  skip_on_windows()

  project <- renv_tests_scope()

  # initialize renv project here
  init()

  # make sure we have renv installed for this test
  libpaths <- the$libpaths[[".libPaths()"]]
  source <- find.package("renv", lib.loc = libpaths)
  renv_imbue_self(project, source = source)

  # make sure autoloader is enabled in this scope
  renv_scope_envvars(RENV_AUTOLOAD_ENABLED = "TRUE")

  # make sure default library paths are visible to child process
  renv_scope_envvars(
    R_LIBS = paste(libpaths, collapse = .Platform$path.sep),
    R_LIBS_USER = NULL,
    R_LIBS_SITE = NULL
  )

  # move to sub-directory
  dir.create("subdir")
  renv_scope_wd("subdir")

  # create a .Rprofile that calls renv::autoload()
  profile <- renv_test_code({
    renv::autoload()
  })

  renv_scope_envvars(R_PROFILE_USER = profile)

  # launch R and see what library paths we got
  output <- renv_system_exec(
    command = R(),
    args    = c("-s", "-e", renv_shell_quote("writeLines(.libPaths()[1])")),
    action  = "testing autoload"
  )

  expected <- .libPaths()[1]
  expect_equal(tail(output, n = length(expected)), expected)

})
