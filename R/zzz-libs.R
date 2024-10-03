
renv_zzz_libs <- function() {
 
  status <- tryCatch(
    renv_zzz_libs_impl(),
    error = identity
  )
  
  if (inherits(status, "error"))
    message("** building without libs")
  
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
  
  if (renv_platform_windows() && getRversion() < "4.2") {
    message("** building without libs")
    return(FALSE)
  }
  
  message("** libs")
  arch <- .Platform$r_arch
  libdir <- paste(c(dir, "libs", if (nzchar(arch)) arch), collapse = "/")
  dir.create(libdir, recursive = TRUE, showWarnings = FALSE)
  renv_ext_compile(libdir)
  TRUE
  
}

renv_zzz_libs()
