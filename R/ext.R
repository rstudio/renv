
renv_ext_enabled <- function() {
  
  # disable on Windows; may be able to re-evaluate in future
  if (renv_platform_windows())
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
  
  # check if we are being invoked via load_all()
  load <- Sys.getenv("DEVTOOLS_LOAD", unset = NA)
  arch <- if (nzchar(.Platform$r_arch)) .Platform$r_arch
  libext <- paste(c(pkgname, "libs", arch), collapse = "/")
  libdir <- file.path(libname, libext)
  
  # use alternate library path for load_all + tests
  if (identical(load, .packageName)) {
    if (!interactive()) {
      root <- tempfile("renv-ext-", tmpdir = dirname(tempdir()))
      libdir <- file.path(root, libext)
    }
    ensure_directory(libdir)
    renv_ext_compile(libdir)
  }
  
  # now try to load it
  soname <- paste0("renv", .Platform$dynlib.ext)
  sofile <- file.path(libdir, soname)
  if (file.exists(sofile)) {
    info <- library.dynam("renv", pkgname, libname)
    the$dll_info <- info
  }

}

renv_ext_compile <- function(libdir) {
  
  if (!renv_ext_enabled())
    return()
  
  soname <- if (renv_platform_windows()) "renv.dll" else "renv.so"
  unlink(file.path(libdir, soname))
  
  srcfiles <- list.files("tools/ext", pattern = "\\.c$", full.names = TRUE)
  file.copy(srcfiles, libdir)
  
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(libdir)
  
  r <- file.path(R.home("bin"), if (.Platform$OS.type == "unix") "R" else "R.exe")
  system2(r, c("CMD", "SHLIB", shQuote(basename(srcfiles))))
  
  oldfiles <- list.files(pattern = "\\.[co]$", full.names = TRUE)
  unlink(oldfiles)
  
}

