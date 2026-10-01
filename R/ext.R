
renv_ext_enabled <- function() {

  # disable on Windows; may be able to re-evaluate in future
  if (renv_platform_windows())
    return(FALSE)

  # disable if we're embedded
  if (!identical(.packageName, "renv"))
    return(FALSE)

  # otherwise, check envvar
  truthy(Sys.getenv("RENV_EXT_ENABLED", unset = "TRUE"))

}

renv_ext_init <- function() {

  if (!renv_ext_enabled() || is.null(the$dll_info))
    return()

  envir <- renv_envir_self()
  symbols <- ls(envir = envir, pattern = "^__ffi__")
  map(symbols, function(symbol) {
    renv_binding_replace(
      envir       = envir,
      symbol      = substring(symbol, 8L),
      replacement = envir[[symbol]]
    )
  })

}

renv_ext_onload <- function(libname, pkgname) {

  if (!renv_ext_enabled())
    return()

  # under devtools::load_all(), the package directory needn't be named after
  # the package (e.g. a git worktree), in which case 'libname' and 'pkgname'
  # don't locate it; fall back to the namespace path, and have library.dynam()
  # find the loaded namespace the same way
  package <- file.path(libname, pkgname)
  if (!file.exists(file.path(package, "DESCRIPTION"))) {
    package <- renv_namespace_path(pkgname)
    libname <- NULL
  }

  libsdir <- renv_package_libsdir(package)

  # if we're being invoked via devtools::load_all(), compile extensions
  # use alternate library path for load_all + tests
  compile <-
    renv_envvar_exists("DEVTOOLS_LOAD") &&
    !renv_envvar_exists("CALLR_IS_RUNNING")

  if (compile) {
    renv_ext_compile(package, libsdir)
  }

  # now try to load it
  soname <- paste0("renv", .Platform$dynlib.ext)
  sofile <- file.path(libsdir, soname)
  if (!file.exists(sofile))
    return()

  # the extensions are optional, so if the library can't be loaded (e.g. it
  # was built for a different architecture), fall back to the R implementations
  info <- tryCatch(
    library.dynam("renv", pkgname, libname),
    error = function(cnd) {
      fmt <- paste(
        "renv's compiled extensions could not be loaded; using R implementations instead.",
        "Reason: %s",
        "Set RENV_EXT_ENABLED = FALSE to suppress this warning.",
        sep = "\n"
      )
      warningf(fmt, conditionMessage(cnd))
      NULL
    }
  )

  the$dll_info <- info

}

renv_ext_compile <- function(package, libsdir = renv_package_libsdir(package)) {

  if (!renv_ext_enabled())
    return()

  soname <- if (renv_platform_windows()) "renv.dll" else "renv.so"
  unlink(file.path(libsdir, soname))

  extdirs <- file.path(package, c("inst/ext", "ext"))
  extdir <- filter(extdirs, file.exists)[[1L]]

  srcfiles <- list.files(extdir, "\\.c$", full.names = TRUE)
  ensure_directory(libsdir)
  file.copy(srcfiles, libsdir)

  renv_scope_wd(libsdir)
  r <- file.path(R.home("bin"), if (.Platform$OS.type == "unix") "R" else "R.exe")
  system2(r, c("CMD", "SHLIB", shQuote(basename(srcfiles))))

  oldfiles <- list.files(pattern = "\\.[co]$", full.names = TRUE)
  unlink(oldfiles)

}

