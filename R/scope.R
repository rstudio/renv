
renv_scope_tempdir <- function(pattern = "renv-tempdir-", .envir = NULL) {

  dir <- tempfile(pattern)
  ensure_directory(dir)
  owd <- setwd(dir)

  .envir <- .envir %||% parent.frame()

  defer({
    setwd(owd)
    unlink(dir, recursive = TRUE)
  }, envir = .envir)

}

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
    return(FALSE)

  call <- renv_error_handler_call()
  options(error = call)

  .envir <- .envir %||% parent.frame()
  defer({
    if (identical(getOption("error"), call))
      options(error = error)
  }, envir = .envir)

  TRUE

}

# used to enforce usage of curl 7.64.1 within the
# renv_paths_extsoft folder when available on Windows

# nocov start
renv_scope_downloader <- function(.envir = NULL) {

  if (!renv_platform_windows())
    return(FALSE)

  if (nzchar(Sys.which("curl")))
    return(FALSE)

  curlroot <- sprintf("curl-%s-win32-mingw", renv_extsoft_curl_version())
  curl <- renv_paths_extsoft(curlroot, "bin/curl.exe")
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
  root <- renv_paths_rtools()
  if (!file.exists(root))
    return(FALSE)

  # get environment variables appropriate for version of Rtools
  vars <- renv_rtools_envvars(root)

  # scope envvars in parent
  .envir <- .envir %||% parent.frame()
  renv_scope_envvars(.list = vars, .envir = .envir)

}
# nocov end

# nocov start
renv_scope_install <- function(.envir = NULL) {

  .envir <- .envir %||% parent.frame()

  if (renv_platform_macos())
    renv_scope_install_macos(.envir)

  if (renv_platform_wsl())
    renv_scope_install_wsl(.envir)

}

renv_scope_install_macos <- function(.envir = NULL) {

  .envir <- .envir %||% parent.frame()

  # get the current compiler
  args <- c("CMD", "config", "CC")
  cc <- system2(R(), args, stdout = TRUE, stderr = TRUE)

  # check to see if we're using the system toolchain
  # (need to be careful since users might put e.g. ccache or other flags
  # into the CC variable)

  # helper for creating regex matching compiler bits
  matches <- function(pattern) {
    regex <- paste("(?:[[:space:]]|^)", pattern, "(?:[[:space:]]|$)", sep = "")
    grepl(regex, cc)
  }

  sysclang <- case(
    matches("/usr/bin/clang") ~ TRUE,
    matches("clang")          ~ Sys.which("clang") == "/usr/bin/clang",
    FALSE
  )

  # check for an appropriate LLVM toolchain -- if it exists, use it
  spec <- renv_equip_macos_spec()
  if (sysclang && !is.null(spec) && file.exists(spec$dst)) {
    path <- paste(file.path(spec$dst, "bin"), Sys.getenv("PATH"), sep = ":")
    renv_scope_envvars(PATH = path, .envir = .envir)
  }

  # generate a custom makevars that should better handle compilation
  # with the system toolchain (or other toolchains)
  makevars <- stack()

  # if we don't have an LLVM toolchain available, then try to generate
  # a Makeconf that shields compilation from usages of '-fopenmp'
  if (sysclang) {

    makeconf <- readLines(file.path(R.home("etc"), "Makeconf"), warn = FALSE)
    mplines <- grep(" -fopenmp", makeconf, fixed = TRUE, value = TRUE)

    # read a user makevars (if any)
    contents <- character()
    mvsite <- Sys.getenv(
      "R_MAKEVARS_SITE",
      unset = file.path(R.home("etc"), "Makevars.site")
    )

    if (file.exists(mvsite))
      contents <- readLines(mvsite, warn = FALSE)

    # override usages of '-fopenmp'
    replaced <- gsub(" -fopenmp", "", mplines, fixed = TRUE)
    amended <- unique(c(contents, replaced))
    makevars$push(amended)

  }

  # write makevars to file
  path <- tempfile("Makevars-")
  contents <- unlist(makevars$data(), recursive = TRUE, use.names = FALSE)
  if (length(contents)) {
    writeLines(contents, con = path)
    renv_scope_envvars(R_MAKEVARS_SITE = path, .envir = .envir)
  }

  TRUE

}

renv_scope_install_wsl <- function(.envir = NULL) {
  .envir <- .envir %||% parent.frame()
  renv_scope_envvars(R_INSTALL_STAGED = "FALSE")
}
# nocov end

renv_scope_restore <- function(..., .envir = NULL) {
  .envir <- .envir %||% parent.frame()
  state <- renv_restore_begin(...)
  defer(renv_restore_end(state), envir = .envir)
}

renv_scope_git_auth <- function(.envir = NULL) {

  .envir <- .envir %||% parent.frame()

  # TODO: not yet implemented for Windows
  if (renv_platform_windows())
    return(FALSE)

  # use GIT_PAT when provided
  pat <- Sys.getenv("GIT_PAT", unset = NA)
  if (!is.na(pat)) {
    renv_scope_envvars(
      GIT_USERNAME = pat,
      GIT_PASSWORD = "x-oauth-basic",
      .envir = .envir
    )
  }

  # only set askpass when GIT_USERNAME + GIT_PASSWORD are set
  user <-
    Sys.getenv("GIT_USERNAME", unset = NA) %NA%
    Sys.getenv("GIT_USER",     unset = NA)

  pass <-
    Sys.getenv("GIT_PASSWORD", unset = NA) %NA%
    Sys.getenv("GIT_PASS",     unset = NA)

  if (is.na(user) || is.na(pass))
    return(FALSE)

  askpass <- system.file("resources/scripts-git-askpass.sh", package = "renv")
  renv_scope_envvars(GIT_ASKPASS = askpass, .envir = .envir)
  return(TRUE)

}

renv_scope_bioconductor <- function(.envir = NULL) {

  .envir <- .envir %||% parent.frame()

  # ensure bioconductor support infrastructure initialized
  renv_bioconductor_init()

  # activate bioconductor repositories in this context
  repos <- getOption("repos")
  biocrepos <- c(renv_bioconductor_repos(), repos)
  renv_scope_options(repos = renv_vector_unique(biocrepos), .envir = .envir)

}
