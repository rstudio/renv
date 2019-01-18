
renv_shim_install_packages <- function(pkgs, ...) {

  # currently we only handle the case where only 'pkgs' was specified
  if (missing(pkgs) || nargs() != 1) {
    args <- mget(ls(envir = environment()), envir = environment())
    return(do.call(utils::install.packages, args))
  }

  # otherwise, we get to handle it
  renv::install(pkgs)

}

renv_shim <- function(shim, sham) {
  formals(shim) <- formals(sham)
  shim
}

renv_shims_init <- function() {

  if (!interactive())
    return()

  while ("renv:shims" %in% search())
    detach("renv:shims")

  envir <- do.call(base::attach, list(NULL, name = "renv:shims"))

  envir$install.packages <- renv_shim(
    renv_shim_install_packages,
    utils::install.packages
  )

}
