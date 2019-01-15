
local({

  # nothing to do if we're running in 'devtools::load_all(".")'
  if (identical(sys.call(1), quote(devtools::load_all("."))) ||
      identical(sys.call(1), quote(devtools::load_all())))
    return()

  # construct renv in R session tempdir for tests
  root <- tempfile("renv-root-")
  dir.create(root, showWarnings = TRUE, mode = "755")
  Sys.setenv(RENV_PATHS_ROOT = root)

})
