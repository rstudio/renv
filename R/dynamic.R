
`_renv_dynamic` <- new.env(parent = emptyenv())

# what a terrible hack
dynamic <- function(key = NULL, expr) {

  parent <- as.character(sys.call(sys.parent())[[1L]])
  id <- paste(c(parent, key), collapse = ":::")

  `_renv_dynamic`[[id]] <- `_renv_dynamic`[[id]] %||% {
    defer(renv_dynamic_cleanup(), envir = sys.frames()[[1L]])
    expr
  }

}

renv_dynamic_cleanup <- function() {

  rm(
    list     = ls(envir = `_renv_dynamic`, all.names = TRUE),
    envir    = `_renv_dynamic`,
    inherits = FALSE
  )

}
