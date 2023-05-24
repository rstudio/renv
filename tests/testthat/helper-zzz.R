
# This is the moral equivalent of 'setup.R', but placed here so that it's
# automatically run after both `devtools::load_all()` and `devtools::test()`.
envir <- if (interactive()) globalenv() else testthat::teardown_env()
renv_tests_setup(envir = envir)
