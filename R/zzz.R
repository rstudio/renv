
.onLoad <- function(libname, pkgname) {

  # cache the path to the actually-installed 'renv'. this is primarily
  # for renv development as devtools::load_all() will sneak in and
  # change the namespace path, changing the behavior of find.package()
  home <- Sys.getenv("RENV_HOME", unset = NA)
  if (is.na(home)) {
    home <- renv_home()
    Sys.setenv(RENV_HOME = home)
  }

  # install renv shims
  renv_shims_init()

  # import cached repositories (if any)
  renv_repos_import()

}
