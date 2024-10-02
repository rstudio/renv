
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
