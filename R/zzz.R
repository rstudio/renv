
.onLoad <- function(libname, pkgname) {

  # cache the path to the actually-installed 'renv'
  renv_global_set("renv", renv_package_find("renv"))

  # install renv shims
  renv_shims_init()

  # import cached repositories (if any)
  renv_repos_import()

}
