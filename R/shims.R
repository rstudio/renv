
`_renv_shims` <- new.env(parent = emptyenv())

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
  install(pkgs)

}

renv_shim_update_packages <- function(lib.loc = NULL, ...) {

  # handle only 0-argument case
  if (nargs() != 0)
    return(renv_delegate(utils::update.packages))

  update(library = lib.loc)

}

renv_shim_remove_packages <- function(pkgs, lib) {

  # handle single-argument case
  if (nargs() != 1)
    return(renv_delegate(utils::remove.packages))

  remove(pkgs)

}

renv_shim <- function(shim, sham) {
  formals(shim) <- formals(sham)
  shim
}

renv_shims_enabled <- function(project) {
  config <- renv_config("shims.enabled", default = TRUE)
  identical(config, TRUE)
}

renv_shims_activate <- function() {

  renv_shims_deactivate()

  install_shim <- renv_shim(renv_shim_install_packages, utils::install.packages)
  assign("install.packages", install_shim, envir = `_renv_shims`)

  update_shim <- renv_shim(renv_shim_update_packages, utils::update.packages)
  assign("update.packages", update_shim, envir = `_renv_shims`)

  remove_shim <- renv_shim(renv_shim_remove_packages, utils::remove.packages)
  assign("remove.packages", remove_shim, envir = `_renv_shims`)

  args <- list(`_renv_shims`, name = "renv:shims", warn.conflicts = FALSE)
  do.call(base::attach, args)

}

renv_shims_deactivate <- function() {
  while ("renv:shims" %in% search())
    detach("renv:shims")
}
