
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
  
  libdir <- file.path(dir, "libs")
  dir.create(libdir, recursive = TRUE, showWarnings = FALSE)
  srcfiles <- list.files("tools/ext", pattern = "\\.c$", full.names = TRUE)
  file.copy(srcfiles, libdir)
  
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(libdir)
  
  message("** extensions")
  system2(rcmd, c("SHLIB", shQuote(basename(srcfiles))))
  
  oldfiles <- list.files(pattern = "\\.[co]$", full.names = TRUE)
  unlink(oldfiles)
  
}

renv_zzz_libs()
