
.onLoad <- function(libname, pkgname) {

  # install renv shims
  renv_shims_init()

  # import cached repositories (if any)
  renv_repos_import()

}
