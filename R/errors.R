
renv_error_format_srcref <- function(call, srcref) {

  srcfile <- attr(srcref, "srcfile", exact = TRUE)

  if (inherits(srcfile, c("srcfilecopy", "srcfilealias"))) {
    start <- srcref[7L]
    end   <- srcref[8L]
  } else {
    start <- srcref[1L]
    end   <- srcref[3L]
  }

  srclines <- getSrcLines(srcfile, start, end)
  index <- regexpr("[^[:space:]]", srclines)
  indent <- min(index)
  code <- substring(srclines, indent)

  if (length(code) >= 8L) {
    simplified <- renv_error_simplify(call)
    if (!identical(simplified, call))
      code <- format(simplified)
  }

  n <- length(code)
  postfix <- sprintf("at %s#%i", basename(srcfile$filename), srcref[1L])
  code[n] <- paste(code[n], postfix)

  code

}

renv_error_simplify <- function(object) {

  case(
    is.function(object)  ~ renv_error_simplify_function(object),
    is.recursive(object) ~ renv_error_simplify_recursive(object),
    TRUE                 ~ object
  )

}

renv_error_simplify_function <- function(object) {
  f <- function() {}
  formals(f) <- formals(object)
  body(f) <- quote({ ... })
  f
}

renv_error_simplify_recursive <- function(object) {

  longcall <-
    is.call(object) &&
    is.symbol(object[[1]]) &&
    object[[1]] == as.name("{") &&
    length(object) >= 8

  if (longcall)
    return(quote(...))

  for (i in seq_along(object))
    if (!is.null(object[[i]]))
      object[[i]] <- renv_error_simplify(object[[i]])

  object

}

renv_error_format <- function(calls, frames) {

  # first, format calls
  formatted <- lapply(calls, function(call) {

    srcref <- attr(call, "srcref", exact = TRUE)
    if (!is.null(srcref)) {
      formatted <- catch(renv_error_format_srcref(call, srcref))
      if (!inherits(formatted, "error"))
        return(formatted)
    }

    if (is.function(call[[1]]))
      return("<condition-handler>(...)")

    format(renv_error_simplify(call))

  })

  # compute prefixes
  numbers <- format(seq_along(formatted))
  prefixes <- sprintf("%s: ", rev(numbers))

  # generate indent
  indent <- paste(rep.int(" ", min(nchar(prefixes))), collapse = "")

  # attach prefixes + indent
  annotated <- uapply(seq_along(formatted), function(i) {
    code <- formatted[[i]]
    prefix <- c(prefixes[[i]], rep.int(indent, length(code) - 1L))
    paste(prefix, code, sep = "")
  })

  header <- "Traceback (most recent calls last):"
  c(header, annotated)

}

renv_error_find <- function(calls, frames) {

  for (i in rev(seq_along(frames))) {

    fn <- sys.function(which = i)
    if (!identical(fn, stop))
      next

    frame <- frames[[i]]
    args <- frame[["args"]]
    if (is.null(args) || empty(args))
      next

    first <- args[[1L]]
    if (!inherits(first, "condition"))
      next

    return(first)

  }

}

renv_error_handler <- function(...) {

  calls <- head(sys.calls(), n = -1L)
  frames <- head(sys.frames(), n = -1L)

  error <- renv_error_find(calls, frames)
  if (identical(error$traceback, FALSE))
    return(character())

  formatted <- renv_error_format(calls, frames)
  writeLines(formatted, con = stderr())

  formatted

}

renv_error_capture <- function(e) {
  calls <- head(sys.calls(), n = -2L)
  frames <- head(sys.frames(), n = -2L)
  traceback <- renv_error_format(calls, frames)
  renv_global_set("traceback", traceback)
}

renv_error_tag <- function(e) {
  e$traceback <- renv_global_get("traceback")
  e
}

renv_error_handler_call <- function() {
  as.call(list(renv_error_handler))
}
