
renv_parse <- function(file = "", text = NULL, encoding = "UTF-8", ...) {

  renv_scope_options(warn = -1L)

  if (is.character(text)) {
    srcfile <- ifelse(nzchar(file), file, NULL)
    return(parse(text = text, encoding = encoding, srcfile = srcfile, ...))
  }

  if (nzchar(file))
    return(parse(file = file, encoding = encoding, ...))

  NULL

}

renv_deparse <- function(expr, width.cutoff = 500L, ...) {
  paste(deparse(expr = expr, width.cutoff = width.cutoff, ...), collapse = " ")
}
