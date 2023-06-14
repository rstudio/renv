
#
# Tools for so-called 'dynamic' values. These are values which are computed
# once, and then memoized for the rest of the currently-executing call.
#
# An exit handler placed in the top-most (renv) environment is then responsible
# for cleaning up any objects cached for the duration of that frame.
#
# This is a useful way to cache results for repeatedly-computed values
# that one can reasonably expect not to change in the duration of a
# particular call.
#

the$dynamic_envir <- NULL
the$dynamic_objects <- new.env(parent = emptyenv())

dynamic <- function(key, value, envir = NULL) {

  # allow opt-out just in case
  enabled <- getOption("renv.dynamic.enabled", default = TRUE)
  if (!enabled)
    return(value)

  # get a unique id for the scope where this function was invoked
  caller <- sys.call(sys.parent())[[1L]]
  if (renv_call_matches(caller, name = ":::"))
    caller <- caller[[3L]]

  # handle cases like FUN
  if (is.null(the$envir_self[[as.character(caller)]])) {
    if (!renv_tests_running()) {
      fmt <- "internal error: dynamic() received unexpected call '%s'"
      stopf(fmt, stringify(sys.call(sys.parent())))
    }
  }

  # just return value if this isn't a valid dynamic scope
  if (!is.symbol(caller)) {
    dlog("dynamic", "invalid dynamic scope '%s'", stringify(sys.call(sys.parent())))
    return(value)
  }

  # make sure we have a dynamic scope active
  the$dynamic_envir <- the$dynamic_envir %||% renv_dynamic_envir(envir)

  # resolve key from variables in the parent frame
  key <- paste(
    names(key),
    map_chr(key, stringify),
    sep = " = ",
    collapse = ", "
  )

  # put it together
  id <- sprintf("%s(%s)", as.character(caller), key)

  # memoize the result of the expression
  the$dynamic_objects[[id]] <- the$dynamic_objects[[id]] %||% {
    dlog("dynamic", "memoizing dynamic value for '%s'", id)
    value
  }

}

renv_dynamic_envir <- function(envir = NULL) {

  envir <- envir %||% renv_dynamic_envir_impl()
  defer(renv_dynamic_reset(), scope = envir)

  dlog("dynamic", "using dynamic environment '%s'", format(envir))
  envir
}

renv_dynamic_envir_impl <- function() {

  for (envir in sys.frames())
    if (identical(parent.env(envir), the$envir_self))
      return(envir)

  stop("internal error: no renv frame available for dynamic call")

}

renv_dynamic_reset <- function() {
  dlog("dynamic", "resetting dynamic objects")
  the$dynamic_envir <- NULL
  renv_envir_clear(the$dynamic_objects)
}
