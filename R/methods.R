
renv_methods_map <- function() {

  list(

    renv_path_normalize = c(
      unix  = "renv_path_normalize_unix",
      win32 = "renv_path_normalize_win32"
    ),

    renv_file_exists = c(
      unix  = "renv_file_exists_unix",
      win32 = "renv_file_exists_win32"
    ),

    renv_file_list_impl = c(
      unix  = "renv_file_list_impl_unix",
      win32 = "renv_file_list_impl_win32"
    ),

    renv_file_broken = c(
      unix  = "renv_file_broken_unix",
      win32 = "renv_file_broken_win32"
    )

  )

}

renv_methods_init <- function() {

  # get list of method mappings
  methods <- renv_methods_map()

  # determine appropriate lookup key for finding alternative
  key <- if (renv_platform_windows()) "win32" else "unix"
  alts <- map(methods, `[[`, key)

  # update methods in namespace
  envir <- renv_envir_self()
  enumerate(alts, function(name, alt) {
    replacement <- eval(parse(text = alt), envir = envir)
    assign(name, replacement, envir = envir)
  })

}
