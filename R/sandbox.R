
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
  syslibs <- renv_path_normalize(syslibs, winslash = "/", mustWork = FALSE)

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
  newlibs <- renv_vector_diff(oldlibs, syslibs)
  renv_libpaths_set(newlibs)

  # protect against user profiles that might try
  # to update the library paths
  renv_sandbox_activate_check(newlibs)

  # return new library paths
  renv_libpaths_all()

}

renv_sandbox_activate_check <- function(libs) {

  envir <- globalenv()

  danger <-
    exists(".First", envir = envir, inherits = FALSE) &&
    !is.na(Sys.getenv("RENV_R_INITIALIZING", unset = NA))

  if (!danger)
    return(FALSE)

  .First <- get(".First", envir = envir, inherits = FALSE)
  wrapper <- function() {

    # scope the library paths as currently defined
    renv_scope_libpaths()

    # call the user-defined .First function
    status <- tryCatch(.First(), error = warning)

    # double-check if we should restore .First (this is extra
    # paranoid but in theory .First could remove itself)
    if (identical(wrapper, get(".First", envir = envir)))
      assign(".First", .First, envir = envir)

    # return result of .First
    invisible(status)

  }

  assign(".First", wrapper, envir = envir)
  return(TRUE)

}

renv_sandbox_deactivate <- function() {

  # get library paths sans .Library, .Library.site
  old <- renv_libpaths_all()
  syslibs <- renv_path_normalize(c(.Library, .Library.site), winslash = "/", mustWork = FALSE)

  # restore old bindings
  base <- .BaseNamespaceEnv
  renv_binding_replace(".Library",      renv_libpaths_system(), envir = base)
  renv_binding_replace(".Library.site", renv_libpaths_site(),   envir = base)

  # update library paths
  new <- renv_vector_diff(old, syslibs)
  renv_libpaths_set(new)

  renv_libpaths_all()

}
