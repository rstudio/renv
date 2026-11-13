
# The registered routines of renv's extension library. These are bound to
# NativeSymbolInfo objects by renv_ext_init() once the library has been
# loaded, so that .Call() doesn't need to resolve the routine by name on
# every call. They're only used via the `__ffi__` wrappers below, which are
# themselves only put into place once the library is loaded.
renv_ffi__enumerate <- NULL
renv_ffi__recurse <- NULL
renv_ffi__renv_version_matrix <- NULL
renv_ffi__renv_call_expect <- NULL
renv_ffi__renv_dependencies_recurse <- NULL

`__ffi__enumerate` <- function(x, f, ..., FUN.VALUE = NULL) {

  f <- match.fun(f)
  
  .Call(
    renv_ffi__enumerate,
    x,
    FUN.VALUE,
    environment()
  )

}

`__ffi__recurse` <- function(object, callback, ...) {
  
  callback <- match.fun(callback)
  
  .Call(
    renv_ffi__recurse,
    object,
    callback,
    environment()
  )
  
}

`__ffi__renv_version_matrix` <- function(versions) {

  .Call(
    renv_ffi__renv_version_matrix,
    versions
  )

}

`__ffi__renv_call_expect` <- function(node, package, methods) {
  
  .Call(
    renv_ffi__renv_call_expect,
    node,
    as.character(package),
    as.character(methods)
  )
  
}

`__ffi__renv_dependencies_recurse` <- function(object, callback) {
  
  symbol <- as.symbol(names(formals(args(callback)))[[1L]])
  expr <- body(callback)
  envir <- new.env(parent = environment(callback))
  
  .Call(
    renv_ffi__renv_dependencies_recurse,
    object,
    symbol,
    expr,
    envir
  )
  
}

