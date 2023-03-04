
# mechanism for running a block of code only once
`_renv_once` <- new.env(parent = emptyenv())

once <- function() {

  call <- sys.call(sys.parent())[[1L]]
  id <- as.character(call)

  once <- `_renv_once`[[id]] %||% TRUE
  `_renv_once`[[id]] <- FALSE

  once

}
