
# on.exit() for arbitrary parent frames
defer <- function(expr, envir = parent.frame()) {

  call <- substitute(
    base::evalq(expr, envir = envir),
    list(expr = substitute(expr), envir = parent.frame())
  )

  do.call(base::on.exit, list(call, add = TRUE), envir = envir)

}

