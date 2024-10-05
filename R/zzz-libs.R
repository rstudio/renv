
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
  package <- Sys.getenv("R_PACKAGE_DIR", unset = getwd())
  renv_ext_compile(package)
  
  TRUE
  
}

renv_zzz_libs()
