active <- readLines(".renv/active", warn = FALSE)
renv::renv_load(active)
