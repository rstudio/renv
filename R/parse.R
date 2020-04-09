
renv_parse <- function(file = "", text = NULL, encoding = "UTF-8", ...) {

  renv_scope_options(warn = -1L)

  # if text was provided, use it (ignoring file)
  if (is.character(text)) {
    srcfile <- if (nzchar(file)) file
    return(parse(text = text, encoding = encoding, srcfile = srcfile, ...))
  }

  # if file is an empty string, ignore it and return NULL
  if (!nzchar(file))
    return(NULL)

  # otherwise, attempt to parse
  return(parse(file = file, encoding = encoding, ...))

}

renv_deparse <- function(expr, width.cutoff = 500L, ...) {
  paste(deparse(expr = expr, width.cutoff = width.cutoff, ...), collapse = " ")
}
