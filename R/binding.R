
renv_binding_init <- function(...) {

  self <- renv_envir_self()
  for (symbol in ls(envir = self))
    if (startswith(symbol, "_"))
      renv_binding_unlock(symbol, self)

}

renv_binding_lock <- function(symbol, envir) {
  .BaseNamespaceEnv$lockBinding(symbol, envir)
}

renv_binding_locked <- function(symbol, envir) {
  .BaseNamespaceEnv$bindingIsLocked(symbol, envir)
}

renv_binding_unlock <- function(symbol, envir) {
  .BaseNamespaceEnv$unlockBinding(symbol, envir)
}

renv_binding_replace <- function(symbol, replacement, envir) {

  # get the original definition
  original <- envir[[symbol]]

  # if the binding is locked, temporarily unlock it
  if (renv_binding_locked(symbol, envir)) {
    renv_binding_unlock(symbol, envir)
    on.exit(renv_binding_lock(symbol, envir), add = TRUE)
  }

  # update the binding
  assign(symbol, replacement, envir = envir)

  # return old definition
  original

}
