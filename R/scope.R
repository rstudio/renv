
renv_scope_auth <- function(record, .envir = NULL) {

  auth <- getOption("renv.auth")

  envvars <- catch(as.character({

    package <- record$Package
    if (is.function(auth))
      return(auth(package, record))

    auth[[package]]

  }))

  if (inherits(envvars, "error")) {
    warning(envvars)
    return(FALSE)
  }

  if (empty(envvars))
    return(FALSE)

  .envir <- .envir %||% parent.frame()
  renv_scope_envvars(.list = envvars, .envir = .envir)
  return(TRUE)

}

renv_scope_libpaths <- function(new, .envir = NULL) {
  .envir <- .envir %||% parent.frame()
  old <- renv_libpaths_set(new)
  defer(renv_libpaths_set(old), envir = .envir)
}

renv_scope_options <- function(..., .envir = NULL) {

  .envir <- .envir %||% parent.frame()

  new <- list(...)
  old <- lapply(names(new), getOption)
  names(old) <- names(new)

  do.call(base::options, new)
  defer(do.call(base::options, old), envir = .envir)

}

renv_scope_locale <- function(category = "LC_ALL", locale = "", .envir = NULL) {
  .envir <- .envir %||% parent.frame()
  saved <- Sys.getlocale(category)
  Sys.setlocale(category, locale)
  defer(Sys.setlocale(category, saved), envir = .envir)
}

renv_scope_envvars <- function(..., .list = NULL, .envir = NULL) {

  .envir <- .envir %||% parent.frame()

  dots <- .list %||% list(...)
  old <- as.list(Sys.getenv(names(dots), unset = NA))
  names(old) <- names(dots)

  Sys.setenv(...)

  defer({
    na <- is.na(old)
    Sys.unsetenv(names(old[na]))
    if (length(old[!na]))
      do.call(Sys.setenv, old[!na])
  }, envir = .envir)

}

renv_scope_sink <- function(file = nullfile(), ..., .envir = NULL) {
  .envir <- .envir %||% parent.frame()
  sink(file = file, ...)
  defer(sink(NULL), envir = parent.frame())
}

renv_scope_error_handler <- function(.envir = NULL) {

  error <- getOption("error")
  if (!is.null(error))
    return()

  .envir <- .envir %||% parent.frame()
  defer(options(error = error), envir = .envir)
  options(error = renv_error_handler)

}

# used to enforce usage of curl 7.64.1 within the
# renv_paths_extsoft folder when available on Windows
renv_scope_downloader <- function(.envir = NULL) {

  if (!renv_platform_windows())
    return(FALSE)

  if (nzchar(Sys.which("curl")))
    return(FALSE)

  curl <- renv_paths_extsoft("curl-7.64.1-win32-mingw/bin/curl.exe")
  if (!file.exists(curl))
    return(FALSE)

  old <- Sys.getenv("PATH", unset = NA)
  if (is.na(old))
    return(FALSE)

  new <- paste(normalizePath(dirname(curl)), old, sep = .Platform$path.sep)

  .envir <- .envir %||% parent.frame()
  renv_scope_envvars(PATH = new, .envir = .envir)

}
