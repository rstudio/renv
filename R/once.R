
# mechanism for running a block of code only once; e.g.
# if (renv_once()) { ... }
`_renv_once` <- new.env(parent = emptyenv())

renv_once <- function() {

  invoker <- sys.function(sys.parent())
  call    <- sys.call(sys.parent())
  envir   <- environment(invoker)

  id <- paste(
    paste(format(envir), collapse = ""),
    paste(format(call), collapse = ""),
    sep = " :: "
  )

  if (exists(id, envir = `_renv_once`, inherits = FALSE))
    return(FALSE)

  assign(id, TRUE, envir = `_renv_once`, inherits = FALSE)
  TRUE
}
