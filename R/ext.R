
renv_ext_compile <- function(libdir) {
  
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
