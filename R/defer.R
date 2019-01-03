
# like on.exit(), but for arbitrary parent frames
defer <- function(expr, envir = parent.frame()) {

  call <- substitute(
    evalq(expr, envir = envir),
    list(expr = substitute(expr), envir = parent.frame())
  )

  force(envir)
  do.call(base::on.exit, list(substitute(call), add = TRUE), envir = envir)

}

