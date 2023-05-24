
# a wrapper around testthat::test_that(), which also tries
# to confirm that the test hasn't mutated any global state
test_that <- function(desc, code) {

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

    if (is.null(path))
      return(NULL)

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

  envvars <- as.list(Sys.getenv())
  envvars <- envvars[grep("^RENV_DEFAULT_", names(envvars), invert = TRUE)]
  envvars <- envvars[grep("^R_PACKRAT_", names(envvars), invert = TRUE)]
  envvars <- envvars[grep("^_R_", names(envvars), invert = TRUE)]
  envvars <- envvars[grep("^CALLR_", names(envvars), invert = TRUE)]
  envvars$RETICULATE_MINICONDA_PYTHON_ENVPATH <- NULL
  envvars$OMP_NUM_THREADS <- NULL
  envvars$OPENBLAS <- NULL
  envvars <- envvars[csort(names(envvars))]

  list(
    libpaths     = .libPaths(),
    connections  = getAllConnections(),
    options      = opts,
    repofiles    = list_files(repopath),
    userfiles    = list_files(userpath),
    envvars      = envvars
  )

}
