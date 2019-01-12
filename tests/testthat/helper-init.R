
if (identical(Sys.getenv("DEVTOOLS_LOAD"), "true")) {
  root <- tempfile("renv-root-")
  dir.create(root, showWarnings = TRUE, mode = "755")
  Sys.setenv(RENV_PATHS_ROOT = root)
}
