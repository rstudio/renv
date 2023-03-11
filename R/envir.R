
renv_envir_self <- function() {
  parent.env(environment())
}

renv_envir_clear <- function(envir) {
  vars <- ls(envir = envir, all.names = TRUE)
  rm(list = vars, envir = envir, inherits = FALSE)
}

renv_envir_unwrap <- function(envir) {
  eapply(envir, function(node) {
    if (is.environment(node))
      renv_envir_unwrap(node)
    else
      node
  })
}
