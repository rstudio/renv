
# installs 'renv' into a special bootstrap directory, which is then
# used by projects on startup so that they can load and use 'renv'
# without requiring a local install
renv_bootstrap <- function(force = FALSE) {

  # construct paths to currently-loaded 'renv' + destination
  # path in the bootstrap library
  source <- renv_global_get("renv")
  target <- renv_paths_bootstrap("renv", renv_package_version("renv"), "renv")
  if (renv_file_same(source, target))
    return(TRUE)

  # copy the directory
  ensure_parent_directory(target)
  renv_file_copy(source, target, overwrite = TRUE)

}
