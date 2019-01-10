
# TODO: install packages by name, record, version, etc.
# maybe treat `foo/bar` as a GitHub install for special case?
install <- function(packages) {

  # create manifest based on state of R libraries
  manifest <- list()
  manifest$R$Package <- uapply(renv_libpaths_all(), function(libpath) {
    renv_snapshot_r_library(libpath, synchronize = FALSE)
  })

  # initialize restore state
  renv_restore_begin(manifest)
  on.exit(renv_restore_end(), add = TRUE)

  # attempt to install each package
  for (package in packages)
    renv_install(package)
}

renv_install <- function(package) {

  # when given the name of a package, just attempt to restore
  # (will try to install latest version from CRAN)
  if (is.character(package))
    renv_restore_install(package)

}
