
# This is the moral equivalent of 'setup.R', but placed here so that it's
# automatically run after both `devtools::load_all()` and `devtools::test()`.
renv_tests_setup(scope = teardown_env())
