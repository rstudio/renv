
.onLoad <- function(libname, pkgname) {
  renv_shims_init()
  renv_patch_init()
}
