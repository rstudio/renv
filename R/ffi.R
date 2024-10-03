
`__ffi__enumerate` <- function(x, f, ..., FUN.VALUE = NULL) {

  f <- match.fun(f)
  if (!is.list(x) && !is.environment(x) && !is.character(x))
    x <- as.list(x)
  
  .Call(
    "renv_ffi__enumerate",
    x,
    FUN.VALUE,
    environment(),
    PACKAGE = "renv"
  )

}

`__ffi__recurse` <- function(object, callback, ...) {
  
  symbol <- as.symbol(names(formals(args(callback)))[[1L]])
  expr <- body(callback)
  envir <- new.env(parent = environment(callback))
  
  .Call(
    "renv_ffi__recurse",
    object,
    symbol,
    expr,
    envir,
    PACKAGE = "renv"
  )
  
}

`__ffi__renv_call_expect` <- function(node, package, methods) {
  
  .Call(
    "renv_ffi__renv_call_expect",
    node,
    as.character(package),
    as.character(methods),
    PACKAGE = "renv"
  )
  
}

`__ffi__renv_dependencies_recurse` <- function(object, callback) {
  
  symbol <- as.symbol(names(formals(args(callback)))[[1L]])
  expr <- body(callback)
  envir <- new.env(parent = environment(callback))
  
  .Call(
    "renv_ffi__renv_dependencies_recurse",
    object,
    symbol,
    expr,
    envir,
    PACKAGE = "renv"
  )
  
}

