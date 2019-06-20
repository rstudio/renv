
renv_binding_replace <- function(symbol, replacement, envir) {
  original <- envir[[symbol]]
  base <- .BaseNamespaceEnv
  base$unlockBinding(symbol, envir)
  assign(symbol, replacement, envir = envir)
  base$lockBinding(symbol, envir)
  original
}
