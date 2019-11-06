
renv_scope_auth <- function(record, .envir = NULL) {

  package <- record$Package
  auth <- renv_options_override("renv.auth", package)

  if (empty(auth))
    return(FALSE)

  envvars <- catch({
    if (is.function(auth))
      auth(record)
    else
      auth
  })

  # warn user if auth appears invalid
  if (inherits(envvars, "error")) {
    warning(envvars)
    return(FALSE)
  }

  if (empty(envvars))
    return(FALSE)

  .envir <- .envir %||% parent.frame()
  renv_scope_envvars(.list = as.list(envvars), .envir = .envir)
  return(TRUE)

}

renv_scope_libpaths <- function(new = .libPaths(), .envir = NULL) {
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

  unset <- map_lgl(dots, is.null)
  Sys.unsetenv(names(dots[unset]))
  if (length(dots[!unset]))
    do.call(Sys.setenv, dots[!unset])

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

# nocov start
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

  new <- paste(renv_path_normalize(dirname(curl)), old, sep = .Platform$path.sep)

  .envir <- .envir %||% parent.frame()
  renv_scope_envvars(PATH = new, .envir = .envir)

}
# nocov end

# nocov start
renv_scope_rtools <- function(.envir = NULL) {

  if (!renv_platform_windows())
    return(FALSE)

  # check for Rtools
  rtools <- renv_paths_rtools()
  if (!file.exists(rtools))
    return(FALSE)

  # add Rtools bin to PATH
  bin <- renv_path_normalize(file.path(rtools, "bin"), winslash = "\\")
  path <- paste(bin, Sys.getenv("PATH"), sep = ";")

  # set BINPREF (note: trailing slash required but file.path()
  # drops trailing slashes on Windows)
  binpref <- paste(rtools, "mingw_$(WIN)/bin/", sep = "/")

  # scope envvars in parent
  .envir <- .envir %||% parent.frame()
  renv_scope_envvars(PATH = path, BINPREF = binpref, .envir = .envir)

}
# nocov end

# nocov start
renv_scope_makevars <- function(.envir = NULL) {

  # currently only required on macOS
  if (!renv_platform_macos())
    return(FALSE)

  # get the current compiler
  args <- c("CMD", "config", "CC")
  compiler <- system2(R(), args, stdout = TRUE, stderr = TRUE)
  resolved <- Sys.which(compiler)

  # if we're not using the system compiler, assume the user has
  # installed their own LLVM toolchain which would likely provide
  # OpenMP support
  if (resolved != "/usr/bin/clang")
    return(FALSE)

  # check for usages of '-fopenmp' in etc/Makeconf
  makeconf <- readLines(file.path(R.home("etc"), "Makeconf"), warn = FALSE)
  mplines <- grep(" -fopenmp", makeconf, fixed = TRUE, value = TRUE)
  if (empty(mplines))
    return(FALSE)

  # read a user makevars (if any)
  contents <- character()
  path <- Sys.getenv(
    "R_MAKEVARS_SITE",
    unset = file.path(R.home("etc"), "Makevars.site")
  )

  if (file.exists(path))
    contents <- readLines(path, warn = FALSE)

  # override usages of '-fopenmp'
  replaced <- gsub(" -fopenmp", "", mplines, fixed = TRUE)
  amended <- unique(c(contents, replaced))

  # set up our own makevars
  .envir <- .envir %||% parent.frame()
  makevars <- tempfile("Makevars-")
  writeLines(amended, con = makevars)

  # tell R to use it
  renv_scope_envvars(R_MAKEVARS_SITE = makevars, .envir = .envir)

}
# nocov end
