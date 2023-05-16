
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

  repopath <- getOption("renv.tests.repopath")
  userpath <- file.path(renv_bootstrap_user_dir(), "library")

  oldlibpaths <- .libPaths()
  oldrepos <- getOption("repos")
  oldconns <- getAllConnections()
  oldopts <- test_options()
  oldrepofiles <- test_list_files(repopath)
  olduserfiles <- test_list_files(userpath)

  call <- sys.call()
  call[[1L]] <- quote(testthat::test_that)
  eval(call, envir = parent.frame())

  reporter <- testthat::get_reporter()

  newlibpaths <- .libPaths()
  if (!identical(reporter$.context, "Sandbox")) {
    test_state_change(desc, "modified libpaths", oldlibpaths, newlibpaths)
  }

  newrepos <- getOption("repos")
  test_state_change(desc, "modified `repos` option", oldrepos, newrepos)

  newrepofiles <- test_list_files(repopath)
  test_state_change(desc, "modified packages in repository", oldrepofiles, newrepofiles)

  newconns <- getAllConnections()
  test_state_change(desc, "leaked connections", oldconns, newconns)

  newopts <- test_options()
  test_state_change(desc, "modified global options", oldopts, newopts)

  newuserfiles <- test_list_files(userpath)
  test_state_change(desc, "left files in user cache directory", olduserfiles, newuserfiles)
}

test_list_files <- function(path) {
  list.files(path = path, all.files  = TRUE, full.names = TRUE, no.. = TRUE)
}

test_options <- function() {
  out <- options()
  out <- out[grep("^renv", names(out), invert = TRUE)]
  out$restart <- NULL
  out
}

test_state_change <- function(desc, msg, old, new) {
  if (!renv_package_available("waldo")) {
    stopf("Test '%s' %s", desc, msg)
  }

  diff <- renv_namespace_load("waldo")$compare(old, new)
  if (length(diff) == 0) {
    return()
  }
  stopf("Test '%s' %s:\n%s", desc, msg, format(diff))
}


expect_error <- function(...) {
  renv_scope_options(renv.tests.verbose = FALSE)
  testthat::expect_error(...)
}

expect_warning <- function(...) {
  renv_scope_options(renv.tests.verbose = FALSE)
  testthat::expect_warning(...)
}
