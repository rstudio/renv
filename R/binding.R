
renv_binding_lock <- function(envir, symbol) {
  .BaseNamespaceEnv$lockBinding(symbol, envir)
}

renv_binding_locked <- function(envir, symbol) {
  .BaseNamespaceEnv$bindingIsLocked(symbol, envir)
}

renv_binding_unlock <- function(envir, symbol) {
  .BaseNamespaceEnv$unlockBinding(symbol, envir)
}

renv_binding_replace <- function(envir, symbol, replacement) {

  # get the original definition
  original <- envir[[symbol]]

  # if the binding is locked, temporarily unlock it
  if (renv_binding_locked(envir, symbol)) {
    defer(renv_binding_lock(envir, symbol))
    renv_binding_unlock(envir, symbol)
  }

  # update the binding
  assign(symbol, replacement, envir = envir)

  # return old definition
  original

}
