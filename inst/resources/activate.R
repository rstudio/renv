active <- readLines("%s/active", warn = FALSE)
renv::renv_load(active)
