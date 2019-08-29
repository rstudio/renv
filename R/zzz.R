
.onLoad <- function(libname, pkgname) {

  renv_patch_init()
  renv_libpaths_init()
  renv_filebacked_init()

  addTaskCallback(renv_repos_init_callback)

}
