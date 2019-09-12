
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

  longexpr <-
    is.call(object) &&
    is.symbol(object[[1]]) &&
    object[[1]] == as.name("{") &&
    length(object) >= 8

  if (longexpr)
    return(quote(...))

  for (i in seq_along(object))
    if (is.call(object[[i]]))
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

  header <- "Traceback (most recent calls first):"
  c(header, annotated)

}

renv_error_handler <- function(...) {
  calls <- head(sys.calls(), n = -1L)
  frames <- head(sys.frames(), n = -1L)
  formatted <- renv_error_format(calls, frames)
  writeLines(formatted, con = stderr())
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
