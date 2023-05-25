
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
  priority <- getOption("renv.sandbox.priority", default = c("base", "recommended"))
  syspkgs <- installed_packages(
    lib.loc = renv_libpaths_system(),
    priority = priority
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

renv_sandbox_lock <- function(sandbox = NULL, project = NULL) {
  sandbox <- sandbox %||% renv_sandbox_path(project = project)
  Sys.chmod(sandbox, mode = "0555")
}

renv_sandbox_locked <- function(sandbox = NULL, project = NULL) {
  sandbox <- sandbox %||% renv_sandbox_path(project = project)
  mode <- suppressWarnings(file.mode(sandbox))
  mode == 365L  # as.integer(as.octmode("0555"))
}

renv_sandbox_unlock <- function(sandbox = NULL, project = NULL) {
  sandbox <- sandbox %||% renv_sandbox_path(project = project)
  Sys.chmod(sandbox, mode = "0755")
}

#' The system library sandbox
#'
#' @description
#' An \R installation can have up to three types of library paths available
#' to the user:
#'
#' - The _user library_, where \R packages downloaded and installed by the
#'   current user are installed. This library path is only visible to that
#'   specific user.
#'
#' - The _site library_, where \R packages maintained by administrators of a
#'   system are installed. This library path, if it exists, is visible to all
#'   users on the system.
#'
#' - The _system library_, where \R packages distributed with \R itself are
#'   installed. This library path is visible to all users on the system.
#'
#' Normally, only so-called "base" and "recommended" packages should be installed
#' in the system library (You can get a list of these packages with
#' `installed.packages(priority = c("base", "recommended"))`). However, it is
#' possible for users and administrators to install packages into the system
#' library, if the filesystem permissions permit them to do so. (This, for
#' example, is the default behavior on macOS.)
#'
#' Because the site and system libraries are visible to all users, having those
#' accessible in renv projects can potentially break isolation -- that is,
#' if a package were updated in the system library, that update would be visible
#' to all \R projects on the system.
#'
#' To help defend against this, renv uses something it calls the "system library
#' sandbox", or the "sandbox", to isolate renv projects from non-"base" packages
#' that are installed into the system library. To accomplish this, when an renv
#' project is loaded, renv will:
#'
#' - Create a new, empty library path (called the "sandbox"),
#'
#' - Link only the "base" packages from the system library into the sandbox,
#'
#' - Mark the sandbox as read-only, so that users are unable to install packages
#'   into this library,
#'
#' - Instruct the \R session to use the "sandbox" as the system library.
#'
#' This process is mostly transparent to the user. However, because the sandbox
#' is read-only, if you later need to remove the sandbox, you'll need to reset
#' file permissions manually; for example, with `renv::sandbox$unlock()`
#'
#' If you'd prefer to keep the sandbox unlocked, you can also set:
#'
#' ```
#' RENV_SANDBOX_LOCKING_ENABLED = FALSE
#' ```
#'
#' in an appropriate startup `.Renviron` or `Renviron.site` file.
#'
#' The sandbox can also be disabled entirely with:
#'
#' ```
#' RENV_CONFIG_SANDBOX_ENABLED = FALSE
#' ```
#'
#' @format NULL
#' @export
sandbox <- list(
  path   = renv_sandbox_path,
  lock   = renv_sandbox_lock,
  locked = renv_sandbox_locked,
  unlock = renv_sandbox_unlock
)
