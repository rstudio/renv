
renv_binding_replace <- function(symbol, replacement, envir) {

  base <- .BaseNamespaceEnv

  # get the original definition
  original <- envir[[symbol]]

  # if the binding is locked, temporarily unlock it
  if (base$bindingIsLocked(symbol, envir)) {
    base$unlockBinding(symbol, envir)
    on.exit(base$lockBinding(symbol, envir), add = TRUE)
  }

  # update the binding
  assign(symbol, replacement, envir = envir)

  # return old definition
  original

}
