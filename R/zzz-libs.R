
renv_zzz_libs <- function() {
 
  status <- tryCatch(
    renv_zzz_libs_impl(),
    error = identity
  )
  
}

renv_zzz_libs_impl <- function() {
  
  if (!installing() || !renv_ext_enabled())
    return(FALSE)
  
  message("** libs")
  dir <- Sys.getenv("R_PACKAGE_DIR", unset = getwd())
  arch <- .Platform$r_arch
  libdir <- paste(c(dir, "libs", if (nzchar(arch)) arch), collapse = "/")
  dir.create(libdir, recursive = TRUE, showWarnings = FALSE)
  renv_ext_compile(libdir)
  TRUE
  
}

renv_zzz_libs()
