
`_renv_sandbox` <- new.env(parent = emptyenv())

renv_sandbox_enabled <- function(project) {
  renv_config("sandbox.enabled", default = renv_platform_unix())
}

renv_sandbox_activate <- function(project = NULL) {

  # check that we haven't already activated
  bindings <- ls(envir = `_renv_sandbox`, all.names = TRUE)
  if (length(bindings))
    return(FALSE)

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
  syslibs <- normalizePath(c(.Library, .Library.site), winslash = "/", mustWork = FALSE)

  # create a temporary library
  syslib <- file.path(tempdir(), "renv-system-library")
  ensure_directory(syslib)

  # find system packages in the system library
  syspkgs <- renv_installed_packages(lib.loc = .Library, priority = "base")

  # link into temporary library
  sources <- with(syspkgs, file.path(LibPath, Package))
  targets <- with(syspkgs, file.path(syslib,  Package))
  names(targets) <- sources
  enumerate(targets, function(source, target) {
    renv_file_link(source, target)
  })

  # override .Library, .Library.site
  base <- .BaseNamespaceEnv
  bindings <- c(.Library = syslib, .Library.site = list(NULL))
  enumerate(bindings, function(binding, replacement) {
    original <- renv_binding_replace(binding, replacement, envir = base)
    assign(binding, original, envir = `_renv_sandbox`)
  })

  # update library paths
  newlibs <- setdiff(oldlibs, syslibs)
  .libPaths(newlibs)

  # protect against user profiles that might try
  # to update the library paths
  renv_sandbox_activate_check(newlibs)

  # return new library paths
  .libPaths()

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

  # check that the sandbox was indeed activated
  bindings <- ls(envir = `_renv_sandbox`, all.names = TRUE)
  if (empty(bindings))
    return(FALSE)

  # get library paths sans .Library, .Library.site
  old <- .libPaths()
  sys <- normalizePath(c(.Library, .Library.site), winslash = "/", mustWork = FALSE)

  # restore old bindings
  base <- .BaseNamespaceEnv
  bindings <- c(".Library", ".Library.site")
  for (binding in c(".Library", ".Library.site")) {
    original <- get(binding, envir = `_renv_sandbox`)
    renv_binding_replace(binding, original, envir = base)
  }
  rm(list = bindings, envir = `_renv_sandbox`)

  # update library paths
  new <- setdiff(old, sys)
  .libPaths(new)

  TRUE

}
