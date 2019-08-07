
renv_format_srcref <- function(srcref) {

  srcfile <- attr(srcref, "srcfile")

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

  n <- length(code)
  postfix <- sprintf("at %s#%i", basename(srcfile$filename), start)
  code[n] <- paste(code[n], postfix)

  code

}

renv_error_handler <- function(...) {

  # first, format calls
  calls <- head(sys.calls(), n = -1L)
  formatted <- lapply(calls, function(call) {

    srcref <- attr(call, "srcref", exact = TRUE)
    if (!is.null(srcref))
      renv_format_srcref(srcref)
    else
      format(call)

  })

  # compute prefixes
  numbers <- format(seq_along(formatted))
  prefixes <- sprintf("%s: ", rev(numbers))
  indent <- paste(rep.int(" ", min(nchar(prefixes))), collapse = "")

  # attach prefixes + indent
  annotated <- map_chr(seq_along(formatted), function(i) {
    code <- formatted[[i]]
    prefix <- c(prefixes[[i]], rep.int(indent, length(code) - 1L))
    paste(prefix, code, sep = "", collapse = "\n")
  })

  header <- "Traceback (most recent calls first):"
  contents <- paste(c(header, annotated), collapse = "\n")
  writeLines(contents, con = stderr())

}
