
renv_ext_init <- function() {
  if (!is.null(the$dll_info)) {
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
}

renv_ext_onload <- function(libname, pkgname) {
 
  # skip on older R installations
  if (renv_platform_windows() && getRversion() < "4.2")
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
  
  soname <- if (renv_platform_windows()) "renv.dll" else "renv.so"
  unlink(file.path(libdir, soname))
  
  srcfiles <- list.files("tools/ext", pattern = "\\.c$", full.names = TRUE)
  file.copy(srcfiles, libdir)
  
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(libdir)
  
  message("** extensions")
  r <- file.path(R.home("bin"), if (.Platform$OS.type == "unix") "R" else "R.exe")
  system2(r, c("CMD", "SHLIB", shQuote(basename(srcfiles))))
  
  oldfiles <- list.files(pattern = "\\.[co]$", full.names = TRUE)
  unlink(oldfiles)
  
}

