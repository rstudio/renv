
# simulate running in R CMD check
Sys.setenv("_R_CHECK_PACKAGE_NAME_" = "renv")

# disable locking in this scope
Sys.setenv(RENV_CONFIG_LOCKING_ENABLED = FALSE)

context <- function(desc) {
  renv_tests_init()
  testthat::context(desc)
}

test_that <- function(desc, code) {

  # clear RENV_PROFILE
  Sys.unsetenv("RENV_PROFILE")

  # skip tests when run on CRAN's macOS machine
  iscran <- !interactive() && !identical(Sys.getenv("NOT_CRAN"), "true")
  testthat::skip_if(iscran && renv_platform_macos())

  track_libpaths = identical(testthat::get_reporter()$.context, "Sandbox")
  state_old <- renv_test_state(track_libpaths)

  call <- sys.call()
  call[[1L]] <- quote(testthat::test_that)
  eval(call, envir = parent.frame())

  state_new <- renv_test_state(track_libpaths)
  state_difff <- renv_namespace_load("waldo")$compare(state_old, state_new)
  if (length(state_difff) > 0) {
    stopf("Test '%s' modified global state\n%s", desc, format(state_difff))
  }

}

test_list_files <- function(path) {
  list.files(path = path, all.files  = TRUE, full.names = TRUE, no.. = TRUE)
}

renv_test_state <- function(libpaths = TRUE) {
  repopath <- getOption("renv.tests.repopath")
  userpath <- file.path(renv_bootstrap_user_dir(), "library")

  opts <- options()
  opts <- opts[grep("^renv", names(opts), invert = TRUE)]
  opts <- opts[grep("^diffobj", names(opts), invert = TRUE)]
  opts$restart <- NULL

  list(
    libpaths = if (libpaths) .libPaths(),
    connection = getAllConnections(),
    options = opts,
    repo_files = test_list_files(repopath),
    user_files = test_list_files(userpath)
  )
}

expect_error <- function(...) {
  renv_scope_options(renv.tests.verbose = FALSE)
  testthat::expect_error(...)
}

expect_warning <- function(...) {
  renv_scope_options(renv.tests.verbose = FALSE)
  testthat::expect_warning(...)
}
