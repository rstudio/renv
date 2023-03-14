
renv_sandbox_init <- function() {

  # check for envvar override
  enabled <- Sys.getenv("RENV_SANDBOX_LOCKING_ENABLED", unset = NA)
  if (!is.na(enabled)) {
    enabled <- truthy(enabled, default = TRUE)
    options(renv.sandbox.locking_enabled = enabled)
  }

}

renv_sandbox_activate <- function(project = NULL) {

  # record start time
  before <- Sys.time()

  # attempt the activation
  status <- catch(renv_sandbox_activate_impl(project))
  if (inherits(status, "error"))
    warning(status)

  # record end time
  after <- Sys.time()

  # check for long elapsed time
  elapsed <- difftime(after, before, units = "secs")

  # if it took too long to activate the sandbox, warn the user
  if (elapsed > 10) {

    fmt <- heredoc("
    renv took longer than expected (%s) to activate the sandbox.

    The sandbox can be disabled by setting:

        RENV_CONFIG_SANDBOX_ENABLED = FALSE

    within an appropriate start-up .Renviron file.

    See `?renv::config` for more details.
    ")


    warningf(fmt, renv_difftime_format(elapsed))

  }

  # return status
  status

}

renv_sandbox_activate_impl <- function(project = NULL, path = NULL) {

  # get current library paths
  oldlibs <- .libPaths()
  syslibs <- c(renv_libpaths_site(), renv_libpaths_system())
  syslibs <- renv_path_normalize(syslibs, winslash = "/", mustWork = FALSE)

  # override .Library.site
  base <- .BaseNamespaceEnv
  renv_binding_replace(".Library.site", NULL, envir = base)

  if (config$sandbox.enabled()) {

    # generate the sandbox
    path <- path %||% renv_sandbox_path(project = project)
    ensure_directory(path)
    renv_sandbox_generate(path)

    # override .Library
    renv_binding_replace(".Library", path, envir = base)

  }

  # update library paths
  newlibs <- renv_vector_diff(oldlibs, syslibs)
  renv_libpaths_set(newlibs)

  if (config$sandbox.enabled()) {

    # protect against user profiles that might try
    # to update the library paths
    renv_sandbox_activate_check(newlibs)

  }

  # return new library paths
  renv_libpaths_all()

}

renv_sandbox_activated <- function() {
  !identical(.Library, renv_libpaths_system())
}

renv_sandbox_activate_check <- function(libs) {

  envir <- globalenv()

  danger <-
    exists(".First", envir = envir, inherits = FALSE) &&
    identical(getOption("renv.autoloader.running"), TRUE)

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

renv_sandbox_generate <- function(sandbox) {

  # lock access to the sandbox
  lockfile <- paste(sandbox, "lock", sep = ".")
  renv_scope_lock(lockfile)

  # make the library temporarily writable
  lock <- getOption("renv.sandbox.locking_enabled") %||% all(
    !renv_package_checking() &&
    !renv_path_within(sandbox, tempdir())
  )

  if (lock) {
    dlog("sandbox", "Temporarily unlocking sandbox.")
    renv_sandbox_unlock(sandbox)
  }

  # find system packages in the system library
  syspkgs <- installed_packages(
    lib.loc = renv_libpaths_system(),
    priority = c("base", "recommended")
  )

  # link into sandbox
  sources <- with(syspkgs, file.path(LibPath, Package))
  targets <- with(syspkgs, file.path(sandbox, Package))
  names(targets) <- sources
  enumerate(targets, renv_file_link, overwrite = TRUE)

  # make the library unwritable again
  if (lock) {
    dlog("sandbox", "Re-locking sandbox.")
    renv_sandbox_lock(sandbox)
  }

  # return sandbox path
  sandbox

}

renv_sandbox_deactivate <- function() {

  # get library paths sans .Library, .Library.site
  old <- renv_libpaths_all()
  syslibs <- renv_path_normalize(c(.Library, .Library.site))

  # restore old bindings
  base <- .BaseNamespaceEnv
  renv_binding_replace(".Library",      renv_libpaths_system(), envir = base)
  renv_binding_replace(".Library.site", renv_libpaths_site(),   envir = base)

  # update library paths
  new <- renv_vector_diff(old, syslibs)
  renv_libpaths_set(new)

  renv_libpaths_all()

}

renv_sandbox_task <- function(...) {

  # check if we're enabled
  if (!renv_sandbox_activated())
    return()

  enabled <- getOption("renv.sandbox.task", default = TRUE)
  if (!enabled)
    return()

  # make sure the sandbox exists
  sandbox <- tail(.libPaths(), n = 1L)
  if (!file.exists(sandbox)) {
    warning("the renv sandbox was deleted; it will be re-generated", call. = FALSE)
    ensure_directory(sandbox)
    renv_sandbox_generate(sandbox)
  }

}

renv_sandbox_path <- function(project = NULL) {
  renv_paths_sandbox(project = project)
}

renv_sandbox_lock <- function(sandbox) {
  Sys.chmod(sandbox, mode = "0555")
}

renv_sandbox_locked <- function(sandbox) {
  mode <- suppressWarnings(file.mode(sandbox))
  mode == 365L  # as.integer(as.octmode("0555"))
}

renv_sandbox_unlock <- function(sandbox) {
  Sys.chmod(sandbox, mode = "0755")
}

