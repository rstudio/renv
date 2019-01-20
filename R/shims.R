
renv_delegate <- function(delegate) {
  call <- sys.call(sys.parent())
  call[[1]] <- substitute(delegate)
  eval(call, envir = parent.frame(2))
}

renv_shim_install_packages <- function(pkgs, ...) {

  # currently we only handle the case where only 'pkgs' was specified
  if (missing(pkgs) || nargs() != 1)
    return(renv_delegate(utils::install.packages))

  # otherwise, we get to handle it
  renv::install(pkgs)

}

renv_shim_update_packages <- function(lib.loc, ...) {

  # handle only 0-argument case
  if (nargs() != 0)
    return(renv_delegate(utils::update.packages))

  # otherwise, check to see what packages require updates, and then install
  old <- as.data.frame(old.packages(), stringsAsFactors = FALSE)
  renv::install(old$Package)

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

  envir$update.packages <- renv_shim(
    renv_shim_update_packages,
    utils::update.packages
  )

}
