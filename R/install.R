
#' Install Packages
#'
#' Install one or more \R packages.
#'
#' `install()` uses the same machinery as [restore()] when installing packages.
#' In particular, this means that the local cache of package installations is
#' used when possible. This helps to avoid re-downloading packages that have
#' already been downloaded before, and re-compiling packages from source when
#' a binary copy of that package is already available.
#'
#' Note that this interface is subject to change -- the goal is to hook into
#' separate package installation backends in the future.
#'
#' @param packages A character vector of \R packages to install. Required
#'   package dependencies (`Depends`, `Imports`, `LinkingTo`) will be installed
#'   as well.
#'
#' @export
install <- function(packages) {

  # create manifest based on state of R libraries
  manifest <- renv_manifest_init()
  manifest$R$Package <- renv_snapshot_r_packages()

  # initialize restore state
  renv_restore_begin(manifest, packages)
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
