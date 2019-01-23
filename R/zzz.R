
.onLoad <- function(libname, pkgname) {

  # cache the path to the actually-installed 'renv'. this is primarily
  # for renv development as devtools::load_all() will sneak in and
  # change the namespace path, changing the behavior of find.package()
  renv_global_set("renv", renv_package_find("renv"))

  # install renv shims
  renv_shims_init()

  # import cached repositories (if any)
  renv_repos_import()

}
