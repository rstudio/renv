
renv_sandbox_init <- function() {

  # check for envvar override
  enabled <- Sys.getenv("RENV_SANDBOX_LOCKING_ENABLED", unset = NA)
  if (!is.na(enabled)) {
    enabled <- truthy(enabled, default = FALSE)
    options(renv.sandbox.locking_enabled = enabled)
  }

  # don't use sandbox in watchdog process
  type <- Sys.getenv("RENV_PROCESS_TYPE")
  if (type == "watchdog-server")
    return()

  # if renv was launched with a sandbox path on the library paths,
  # then immediately try to activate the sandbox
  # https://github.com/rstudio/renv/issues/1565
  for (libpath in .libPaths()) {
    if (file.exists(file.path(libpath, ".renv-sandbox"))) {
      renv_sandbox_activate_impl(sandbox = libpath)
      break
    }
  }

}

renv_sandbox_activate <- function(project = NULL) {

  # record start time
  before <- Sys.time()

  # attempt the activation
  status <- catch(renv_sandbox_activate_impl(project))
  if (inherits(status, "error"))
    warnify(status)

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

renv_sandbox_activate_impl <- function(project = NULL, sandbox = NULL) {

  # lock access to the sandbox
  if (config$sandbox.enabled()) {
    sandbox <- sandbox %||% renv_sandbox_path(project = project)
    lockfile <- paste(sandbox, "lock", sep = ".")
    ensure_parent_directory(lockfile)
    renv_scope_lock(lockfile)
    ensure_directory(sandbox)
  }

  # get current library paths
  oldlibs <- .libPaths()
  syslibs <- c(renv_libpaths_site(), renv_libpaths_system())
  syslibs <- renv_path_normalize(syslibs)

  # override .Library.site
  base <- .BaseNamespaceEnv
  renv_binding_replace(base, ".Library.site", NULL)

  # generate sandbox
  if (config$sandbox.enabled()) {
    renv_sandbox_generate(sandbox)
    renv_binding_replace(base, ".Library", sandbox)
  }

  # update library paths
  newlibs <- renv_vector_diff(oldlibs, syslibs)
  renv_libpaths_set(newlibs)

  # protect against user profiles that might update library paths
  if (config$sandbox.enabled())
    renv_sandbox_activate_check(newlibs)

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
    status <- tryCatch(.First(), error = warnify)

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

  # make the library temporarily writable
  lock <- getOption("renv.sandbox.locking_enabled", default = TRUE)

  if (lock) {
    dlog("sandbox", "unlocking sandbox")
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
  enumerate(targets, function(source, target) {
    if (!renv_file_same(source, target))
      renv_file_link(source, target, overwrite = TRUE)
  })

  # create marker indicating this is a sandbox
  # (or, if it already exists, re-create it and update its ctime / mtime)
  marker <- file.path(sandbox, ".renv-sandbox")
  file.create(marker)

  # update mtime on the sandbox itself as well
  Sys.setFileTime(sandbox, time = Sys.time())

  # make the library unwritable again
  if (lock) {
    dlog("sandbox", "locking sandbox")
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
  renv_binding_replace(base, ".Library",      renv_libpaths_system())
  renv_binding_replace(base, ".Library.site", renv_libpaths_site())

  # update library paths
  new <- renv_vector_diff(old, syslibs)
  renv_libpaths_set(new)

  renv_libpaths_all()

}

renv_sandbox_task <- function(...) {

  # check if we're enabled
  if (!renv_sandbox_activated())
    return()

  # allow opt-out if necessary
  enabled <- getOption("renv.sandbox.task", default = TRUE)
  if (!enabled)
    return()

  # get sandbox path
  sandbox <- tail(.libPaths(), n = 1L)

  # make sure it exists
  if (!file.exists(sandbox)) {
    warning("the renv sandbox was deleted; it will be re-generated", call. = FALSE)
    ensure_directory(sandbox)
    renv_sandbox_generate(sandbox)
  }

  # update the sandbox write time / mtime
  Sys.setFileTime(sandbox, time = Sys.time())

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

#' The default library sandbox
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
#' - The _default library_, where \R packages distributed with \R itself are
#'   installed. This library path is visible to all users on the system.
#'
#' Normally, only so-called "base" and "recommended" packages should be installed
#' in the default library. (You can get a list of these packages with
#' `installed.packages(priority = c("base", "recommended"))`). However, it is
#' possible for users and administrators to install packages into the default
#' library, if the filesystem permissions permit them to do so. (This, for
#' example, is the default behavior on macOS.)
#'
#' Because the site and default libraries are visible to all users, having those
#' accessible in renv projects can potentially break isolation -- that is,
#' if a package were updated in the default library, that update would be visible
#' to all \R projects on the system.
#'
#' To help defend against this, renv uses something called the "sandbox" to
#' isolate renv projects from non-"base" packages that are installed into the
#' default library. When an renv project is loaded, renv will:
#'
#' - Create a new, empty library path (called the "sandbox"),
#'
#' - Link only the "base" and "recommended" packages from the default library
#'   into the sandbox,
#'
#' - Mark the sandbox as read-only, so that users are unable to install packages
#'   into this library,
#'
#' - Instruct the \R session to use the "sandbox" as the default library.
#'
#' This process is mostly transparent to the user. However, because the sandbox
#' is read-only, if you later need to remove the sandbox, you'll need to reset
#' file permissions manually; for example, with `renv::sandbox$unlock()`.
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
#' The sandbox library path can also be configured using the `RENV_PATHS_SANDBOX`
#' environment variable: see [paths] for more details.
#'
#' @format NULL
#' @export
sandbox <- list(
  path   = renv_sandbox_path,
  lock   = renv_sandbox_lock,
  locked = renv_sandbox_locked,
  unlock = renv_sandbox_unlock
)
