
renv_verbose <- function() {

  verbose <- getOption("renv.verbose")
  if (!is.null(verbose))
    return(as.logical(verbose))

  verbose <- Sys.getenv("RENV_VERBOSE", unset = NA)
  if (!is.na(verbose))
    return(as.logical(verbose))

  if (is_testing())
    return(FALSE)

  interactive() || !renv_tests_running()

}

# NOTE: Prefer using 'is_testing()' to 'renv_tests_running()' for behavior
# that should apply regardless of the package currently being tested.
#
# renv_tests_running() is appropriate when running renv's own tests.
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}
