
renv_scope_tempdir <- function(pattern = "renv-tempdir-",
                               tmpdir = tempdir(),
                               umask = NULL,
                               scope = parent.frame())
{
  dir <- renv_scope_tempfile(pattern = pattern, tmpdir = tmpdir, scope = scope)
  ensure_directory(dir, umask = umask)

  renv_scope_wd(dir, scope = scope)
  dir
}

renv_scope_auth <- function(record, scope = parent.frame()) {

  package <- if (is.list(record)) record$Package else record
  auth <- renv_options_override("renv.auth", package, extra = record)

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

  renv_scope_envvars(list = as.list(envvars), scope = scope)
  return(TRUE)

}

renv_scope_libpaths <- function(new = .libPaths(), scope = parent.frame()) {
  old <- renv_libpaths_set(new)
  defer(renv_libpaths_set(old), scope = scope)
}

renv_scope_options <- function(..., scope = parent.frame()) {
  new <- list(...)
  old <- options(new)
  defer(options(old), scope = scope)
}

renv_scope_locale <- function(category = "LC_ALL", locale = "", scope = parent.frame()) {
  saved <- Sys.getlocale(category)
  Sys.setlocale(category, locale)
  defer(Sys.setlocale(category, saved), scope = scope)
}

renv_scope_envvars <- function(..., list = NULL, scope = parent.frame()) {

  dots <- list %||% list(...)
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
  }, scope = scope)

}

renv_scope_error_handler <- function(scope = parent.frame()) {

  error <- getOption("error")
  if (!is.null(error))
    return(FALSE)

  call <- renv_error_handler_call()
  options(error = call)

  defer({
    if (identical(getOption("error"), call))
      options(error = error)
  }, scope = scope)

  TRUE

}

# used to enforce usage of curl 7.64.1 within the
# renv_paths_extsoft folder when available on Windows

# nocov start
renv_scope_downloader <- function(scope = parent.frame()) {

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

  renv_scope_envvars(PATH = new, scope = scope)

}
# nocov end

# nocov start
renv_scope_rtools <- function(scope = parent.frame()) {

  if (!renv_platform_windows())
    return(FALSE)

  # check for Rtools
  root <- renv_paths_rtools()
  if (!file.exists(root))
    return(FALSE)

  # get environment variables appropriate for version of Rtools
  vars <- renv_rtools_envvars(root)

  # scope envvars in parent
  renv_scope_envvars(list = vars, scope = scope)

}
# nocov end

# nocov start
renv_scope_install <- function(scope = parent.frame()) {

  if (renv_platform_macos())
    renv_scope_install_macos(scope)

  if (renv_platform_wsl())
    renv_scope_install_wsl(scope)

}

renv_scope_install_macos <- function(scope = parent.frame()) {

  # check that we have command line tools available before invoking
  # R CMD config, as this might fail otherwise
  if (once()) {
    if (!renv_xcode_available()) {
      message("- macOS is reporting that command line tools (CLT) are not installed.")
      message("- Run 'xcode-select --install' to install command line tools.")
      message("- Without CLT, attempts to install packages from sources may fail.")
    }
  }

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
    renv_scope_envvars(PATH = path, scope = scope)
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
    renv_scope_envvars(R_MAKEVARS_SITE = path, scope = scope)
  }

  TRUE

}

renv_scope_install_wsl <- function(scope = parent.frame()) {
  renv_scope_envvars(R_INSTALL_STAGED = "FALSE", scope = scope)
}
# nocov end

renv_scope_restore <- function(..., scope = parent.frame()) {
  state <- renv_restore_begin(...)
  defer(renv_restore_end(state), scope = scope)
}

renv_scope_git_auth <- function(scope = parent.frame()) {

  # try and tell git to be non-interactive by default
  if (renv_platform_windows()) {
    renv_scope_envvars(
      GIT_TERMINAL_PROMPT = "0",
      scope              = scope
    )
  } else {
    renv_scope_envvars(
      GIT_TERMINAL_PROMPT = "0",
      GIT_ASKPASS         = "/bin/echo",
      scope              = scope
    )
  }

  # use GIT_PAT when provided
  pat <- Sys.getenv("GIT_PAT", unset = NA)
  if (!is.na(pat)) {
    renv_scope_envvars(
      GIT_USERNAME = pat,
      GIT_PASSWORD = "x-oauth-basic",
      scope = scope
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

  askpass <- if (renv_platform_windows())
    system.file("resources/scripts-git-askpass.cmd", package = "renv")
  else
    system.file("resources/scripts-git-askpass.sh", package = "renv")

  renv_scope_envvars(GIT_ASKPASS = askpass, scope = scope)
  return(TRUE)

}

renv_scope_bioconductor <- function(project = NULL,
                                    version = NULL,
                                    scope = parent.frame())
{
  # get current repository
  repos <- getOption("repos")

  # remove old / stale bioc repositories
  stale <- grepl("Bioc", names(repos))
  repos <- repos[!stale]

  # retrieve bioconductor repositories appropriate for this project
  biocrepos <- renv_bioconductor_repos(project = project, version = version)

  # put it all together
  allrepos <- c(repos, biocrepos)

  # activate repositories in this context
  renv_scope_options(repos = renv_vector_unique(allrepos), scope = scope)
}

renv_scope_lock <- function(path = NULL, scope = parent.frame()) {
  renv_lock_acquire(path)
  defer(renv_lock_release(path), scope = scope)
}

renv_scope_trace <- function(what, tracer, scope = parent.frame()) {

  call <- sys.call()
  call[[1L]] <- base::trace
  call[["print"]] <- FALSE
  defer(suppressMessages(untrace(substitute(what))), scope = scope)

  suppressMessages(eval(call, envir = parent.frame()))

}


renv_scope_binding <- function(envir, symbol, replacement, scope = parent.frame()) {
  if (exists(symbol, envir, inherits = FALSE)) {
    old <- renv_binding_replace(envir, symbol, replacement)
    defer(renv_binding_replace(envir, symbol, old), scope = scope)
  } else {
    assign(symbol, replacement, envir)
    defer(rm(list = symbol, envir = envir, inherits = FALSE), scope = scope)
  }
}

renv_scope_tempfile <- function(pattern = "renv-tempfile-",
                                tmpdir  = tempdir(),
                                fileext = "",
                                scope  = parent.frame())
{
  path <- renv_path_normalize(tempfile(pattern, tmpdir, fileext))
  defer(unlink(path, recursive = TRUE, force = TRUE), scope = scope)
  invisible(path)
}

renv_scope_umask <- function(umask, scope = parent.frame()) {
  oldmask <- Sys.umask(umask)
  defer(Sys.umask(oldmask), scope = scope)
  invisible(oldmask)
}

renv_scope_wd <- function(dir = getwd(), scope = parent.frame()) {
  owd <- setwd(dir)
  defer(setwd(owd), scope = scope)
  invisible(owd)
}

renv_scope_sandbox <- function(scope = parent.frame()) {
  sandbox <- renv_sandbox_activate()
  defer(renv_sandbox_deactivate(), scope = scope)
  invisible(sandbox)
}

renv_scope_biocmanager <- function(scope = parent.frame()) {

  # silence BiocManager messages when setting repositories
  renv_scope_options(BiocManager.check_repositories = FALSE, scope = scope)

  # R-devel (4.4.0) warns when BiocManager calls .make_numeric_version() without
  # a character argument, so just suppress those warnings in this scope
  #
  # https://github.com/wch/r-source/commit/1338a95618ddcc8a0af77dc06e4018625de06ec3
  renv_scope_options(warn = -1L, scope = scope)

  # return reference to BiocManager namespace
  renv_namespace_load("BiocManager")

}

renv_scope_caution <- function(value) {
  renv_scope_options(
    renv.caution.verbose = value,
    scope = parent.frame()
  )
}

renv_scope_verbose_if <- function(value, scope = parent.frame()) {
  if (value) {
    renv_scope_options(
      renv.verbose = TRUE,
      scope = scope
    )
  }
}
