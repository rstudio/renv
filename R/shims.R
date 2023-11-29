
the$shims <- new.env(parent = emptyenv())

# determine whether we can safely handle a call to install.packages()
renv_shim_install_packages_compatible <- function(matched) {
  ok <- c("", "dependencies", "pkgs", "lib", "repos", "type")
  unhandled <- setdiff(names(matched), ok)
  length(unhandled) == 0L
}

renv_shim_install_packages <- function(pkgs, ...) {

  # place Rtools on PATH
  renv_scope_rtools()

  # check for compatible calls
  matched <- match.call(utils::install.packages)
  if (!renv_shim_install_packages_compatible(matched)) {
    call <- sys.call()
    call[[1L]] <- quote(utils::install.packages)
    return(eval(call, envir = parent.frame()))
  }

  # otherwise, invoke our own installer
  call <- sys.call()
  call[[1L]] <- quote(renv::install)

  # fix up names
  aliases <- list(lib = "library")
  idx <- omit_if(match(names(aliases), names(call)), is.na)
  names(call)[idx] <- aliases[idx]

  # evaluate call
  eval(call, envir = parent.frame())

}

renv_shim_update_packages <- function(lib.loc = NULL, ...) {

  # handle only 0-argument case
  if (nargs() != 0) {
    call <- sys.call()
    call[[1L]] <- quote(utils::update.packages)
    return(eval(call, envir = parent.frame()))
  }

  update(library = lib.loc)

}

renv_shim_remove_packages <- function(pkgs, lib) {

  # handle single-argument case
  if (nargs() != 1) {
    call <- sys.call()
    call[[1L]] <- quote(utils::remove.packages)
    return(eval(call, envir = parent.frame()))
  }

  remove(pkgs)

}

renv_shim_create <- function(shim, sham) {
  formals(shim) <- formals(sham)
  shim
}

renv_shims_enabled <- function(project) {
  config$shims.enabled()
}

renv_shims_activate <- function() {

  renv_shims_deactivate()

  install_shim <- renv_shim_create(renv_shim_install_packages, utils::install.packages)
  assign("install.packages", install_shim, envir = the$shims)

  update_shim <- renv_shim_create(renv_shim_update_packages, utils::update.packages)
  assign("update.packages", update_shim, envir = the$shims)

  remove_shim <- renv_shim_create(renv_shim_remove_packages, utils::remove.packages)
  assign("remove.packages", remove_shim, envir = the$shims)

  args <- list(the$shims, name = "renv:shims", warn.conflicts = FALSE)
  do.call(base::attach, args)

}

renv_shims_deactivate <- function() {
  while ("renv:shims" %in% search())
    detach("renv:shims")
}
