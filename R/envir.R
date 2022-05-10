
renv_envir_self <- function() {
  parent.env(environment())
}

renv_envir_unwrap <- function(envir) {
  eapply(envir, function(node) {
    if (is.environment(node))
      renv_envir_unwrap(node)
    else
      node
  })
}
