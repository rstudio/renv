
# mechanism for running a block of code only once
the$once <- new.env(parent = emptyenv())

once <- function() {

  call <- sys.call(sys.parent())[[1L]]
  id <- as.character(call)

  once <- the$once[[id]] %||% TRUE
  the$once[[id]] <- FALSE

  once

}
