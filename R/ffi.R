
`__ffi__renv_dependencies_recurse` <- function(object, callback) {
  
  symbol <- as.symbol(names(formals(args(callback)))[[1L]])
  envir <- new.env(parent = environment(callback))
  expr <- body(callback)
  
  .Call(
    "renv_ffi_recurse",
    object,
    symbol,
    expr,
    envir,
    PACKAGE = .packageName
  )
  
}

`__ffi__renv_call_expect` <- function(node, package, methods) {
  .Call(
    "renv_ffi_call_expect",
    node,
    as.character(package),
    as.character(methods),
    PACKAGE = .packageName
  )
}
