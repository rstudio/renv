
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

  # record global state before test execution
  before <- renv_test_state()

  # run the test
  call <- sys.call()
  call[[1L]] <- quote(testthat::test_that)
  eval(call, envir = parent.frame())

  # record global state after test execution
  after <- renv_test_state()

  # check for unexpected changes
  diffs <- waldo::compare(before, after)
  if (length(diffs)) {
    fdiffs <- paste(format(diffs), collapse = "\n\n")
    stopf("Test '%s' has modified global state:\n%s\n", desc, fdiffs)
  }

}

renv_test_state <- function() {

  list_files <- function(path) {
    list.files(
      path = path,
      all.files = TRUE,
      full.names = TRUE,
      no.. = TRUE
    )
  }

  repopath <- getOption("renv.tests.repopath")
  userpath <- file.path(renv_bootstrap_user_dir(), "library")

  opts <- options()
  opts <- opts[grep("^diffobj", names(opts), invert = TRUE)]
  opts$ambiguousMethodSelection <- NULL
  opts$restart <- NULL
  opts$repos[opts$repos == "@CRAN@"] <- "https://cloud.r-project.org"
  opts <- opts[csort(names(opts))]

  list(
    libpaths   = .libPaths(),
    connection = getAllConnections(),
    options    = opts,
    repo_files = if (!is.null(repopath)) list_files(repopath),
    user_files = list_files(userpath)
  )
}
