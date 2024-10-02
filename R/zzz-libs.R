
renv_zzz_libs <- function() {
 
  tryCatch(
    renv_zzz_libs_impl(),
    error = function(cnd) {
      message("** building without extensions")
    }
  )
  
}

renv_zzz_libs_impl <- function() {
  
  pkg <- Sys.getenv("R_INSTALL_PKG", unset = NA)
  if (is.na(pkg))
    return(FALSE)
  
  dir <- Sys.getenv("R_PACKAGE_DIR", unset = NA)
  if (is.na(dir))
    return(FALSE)
  
  rcmd <- Sys.getenv("R_CMD", unset = NA)
  if (is.na(rcmd))
    return(FALSE)
  
  arch <- .Platform$r_arch
  libdir <- paste(c(dir, "libs", if (nzchar(arch)) arch), collapse = "/")
  dir.create(libdir, recursive = TRUE, showWarnings = FALSE)
  
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

renv_zzz_libs()
