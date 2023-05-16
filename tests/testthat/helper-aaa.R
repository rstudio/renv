
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

  state_old <- renv_test_state()

  call <- sys.call()
  call[[1L]] <- quote(testthat::test_that)
  eval(call, envir = parent.frame())

  state_new <- renv_test_state()
  state_diff <- renv_namespace_load("waldo")$compare(state_old, state_new)
  if (length(state_diff) > 0) {
    diffs <- paste0(format(state_diff), collapse = "\n\n")
    stopf("Test '%s' modified global state\n%s", desc, diffs)
  }

}

test_list_files <- function(path) {
  list.files(path = path, all.files  = TRUE, full.names = TRUE, no.. = TRUE)
}

renv_test_state <- function() {
  repopath <- getOption("renv.tests.repopath")
  userpath <- file.path(renv_bootstrap_user_dir(), "library")

  opts <- options()
  opts <- opts[grep("^diffobj", names(opts), invert = TRUE)]
  opts$ambiguousMethodSelection <- NULL
  opts$restart <- NULL

  list(
    libpaths = .libPaths(),
    connection = getAllConnections(),
    options = opts,
    repo_files = test_list_files(repopath),
    user_files = test_list_files(userpath)
  )
}
