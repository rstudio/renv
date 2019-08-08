
renv_sandbox_enabled <- function(project) {
  renv_config("sandbox.enabled", default = renv_platform_unix())
}

renv_sandbox_activate <- function(project = NULL) {

  # attempt the activation
  status <- catch(renv_sandbox_activate_impl(project))
  if (inherits(status, "error"))
    warning(status)

  # return status
  status

}

renv_sandbox_activate_impl <- function(project) {

  # get current library paths
  oldlibs <- .libPaths()
  syslibs <- c(renv_libpaths_site(), renv_libpaths_system())
  syslibs <- normalizePath(syslibs, winslash = "/", mustWork = FALSE)

  # create a temporary library
  sandbox <- file.path(tempdir(), "renv-system-library")
  ensure_directory(sandbox)

  # find system packages in the system library
  syspkgs <- renv_installed_packages(
    lib.loc = renv_libpaths_system(),
    priority = c("base", "recommended")
  )

  # link into temporary library
  sources <- with(syspkgs, file.path(LibPath, Package))
  targets <- with(syspkgs, file.path(sandbox, Package))
  names(targets) <- sources
  enumerate(targets, function(source, target) {
    renv_file_link(source, target)
  })

  # override .Library, .Library.site
  base <- .BaseNamespaceEnv
  renv_binding_replace(".Library",      sandbox, envir = base)
  renv_binding_replace(".Library.site", NULL,    envir = base)

  # update library paths
  newlibs <- setdiff(oldlibs, syslibs)
  renv_libpaths_set(newlibs)

  # protect against user profiles that might try
  # to update the library paths
  renv_sandbox_activate_check(newlibs)

  # return new library paths
  renv_libpaths_all()

}

renv_sandbox_activate_check <- function(libs) {

  danger <-
    exists(".First", envir = globalenv(), inherits = FALSE) &&
    !is.na(Sys.getenv("RENV_R_INITIALIZING", unset = NA))

  if (!danger)
    return(FALSE)

  genv <- globalenv()
  oldfirst <- get(".First", envir = genv, inherits = FALSE)
  wrapper <- function() {

    # call .First and then ensure libpaths are set
    status <- oldfirst()
    .libPaths(libs)

    # double-check if we should restore .First (this is extra
    # paranoid but in theory .First could remove itself)
    newfirst <- genv[[".First"]]
    if (identical(newfirst, wrapper))
      assign(".First", oldfirst, envir = genv)

    # return result of .First
    invisible(status)

  }

  assign(".First", envir = genv, wrapper)
  return(TRUE)

}

renv_sandbox_deactivate <- function() {

  # get library paths sans .Library, .Library.site
  old <- renv_libpaths_all()
  syslibs <- normalizePath(c(.Library, .Library.site), winslash = "/", mustWork = FALSE)

  # restore old bindings
  base <- .BaseNamespaceEnv
  renv_binding_replace(".Library",      renv_libpaths_system(), envir = base)
  renv_binding_replace(".Library.site", renv_libpaths_site(),   envir = base)

  # update library paths
  new <- setdiff(old, syslibs)
  renv_libpaths_set(new)

  renv_libpaths_all()

}
