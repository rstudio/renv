
.onLoad <- function(libname, pkgname) {
  renv_shims_init()
  renv_patch_init()
  addTaskCallback(renv_repos_init_callback)
}
