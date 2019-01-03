
defer <- function(expr, envir = parent.frame()) {

  call <- substitute(
    evalq(expr, envir = envir),
    list(expr = substitute(expr), envir = parent.frame())
  )

  do.call(base::on.exit, list(call, add = TRUE), envir = envir)

}

