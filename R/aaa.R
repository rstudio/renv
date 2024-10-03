
# global variables
the <- new.env(parent = emptyenv())
the$paths <- new.env(parent = emptyenv())

# detect if we're running on CI
ci <- function() {
  !is.na(Sys.getenv("CI", unset = NA))
}

# check if the renv autoloader is running
autoloading <- function() {
  getOption("renv.autoloader.running", default = FALSE)
}

# detect if we're running within R CMD build
building <- function() {
  nzchar(Sys.getenv("R_CMD")) &&
    grepl("Rbuild", basename(dirname(getwd())), fixed = TRUE)
}

# detect if we're running within R CMD INSTALL
installing <- function() {
  nzchar(Sys.getenv("R_INSTALL_PKG"))
}

# are we running code within R CMD check?
checking <- function() {
  "CheckExEnv" %in% search() ||
    renv_envvar_exists("_R_CHECK_PACKAGE_NAME_") ||
    renv_envvar_exists("_R_CHECK_SIZE_OF_TARBALL_")
}

# NOTE: Prefer using 'testing()' to 'renv_tests_running()' for behavior
# that should apply regardless of the package currently being tested.
#
# 'renv_tests_running()' is appropriate when running renv's own tests.
testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}
