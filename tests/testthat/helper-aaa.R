
context <- function(desc) {
  renv_tests_init()
  testthat::context(desc)
}

test_that <- function(desc, code) {

  cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
  testthat::skip_if(cran && renv_platform_macos())

  oldlibpaths <- .libPaths()
  oldrepos <- getOption("repos")

  call <- sys.call()
  call[[1]] <- quote(testthat::test_that)
  eval(call, envir = parent.frame())

  newlibpaths <- .libPaths()
  reporter <- testthat::get_reporter()

  ok <-
    identical(reporter$.context, "Sandbox") ||
    identical(oldlibpaths, newlibpaths)

  if (!ok) {
    writeLines(c("", oldlibpaths, "", newlibpaths))
    stopf("test %s has corrupted libpaths", shQuote(desc))
  }

  newrepos <- getOption("repos")
  ok <- identical(oldrepos, newrepos)
  if (!ok) {
    writeLines(c("", oldrepos, "", newrepos))
    stopf("test %s has corrupted repos", shQuote(desc))
  }

}
