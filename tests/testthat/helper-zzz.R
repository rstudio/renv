
# This is the moral equivalent of 'setup.R', but placed here
# so that it's automatically run after devtools::load_all().
if (interactive()) {
  renv_tests_setup(envir = globalenv())
}
