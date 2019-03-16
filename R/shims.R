
renv_delegate <- function(delegate) {
  call <- sys.call(sys.parent())
  call[[1]] <- substitute(delegate)
  eval(call, envir = parent.frame(2))
}

renv_shim_install_packages <- function(pkgs, ...) {

  # currently we only handle the case where only 'pkgs' was specified
  if (missing(pkgs) || nargs() != 1)
    return(renv_delegate(install.packages))

  # otherwise, we get to handle it
  install(pkgs)

}

renv_shim_update_packages <- function(lib.loc, ...) {

  # handle only 0-argument case
  if (nargs() != 0)
    return(renv_delegate(update.packages))

  # otherwise, check to see what packages require updates, and then install
  old <- as.data.frame(old.packages(), stringsAsFactors = FALSE)
  install(old$Package)

}

renv_shim_remove_packages <- function(pkgs, lib) {

  # handle single-argument case
  if (nargs() != 1)
    return(renv_delegate(remove.packages))

  remove(pkgs)

}

renv_shim <- function(shim, sham) {
  formals(shim) <- formals(sham)
  shim
}

renv_shims_init <- function() {

  while ("renv:shims" %in% search())
    detach("renv:shims")

  envir <- do.call(base::attach, list(NULL, name = "renv:shims"))

  envir$install.packages <- renv_shim(
    renv_shim_install_packages,
    install.packages
  )

  envir$update.packages <- renv_shim(
    renv_shim_update_packages,
    update.packages
  )

  envir$remove.packages <- renv_shim(
    renv_shim_remove_packages,
    remove.packages
  )

}
