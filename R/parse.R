
renv_parse <- function(file = "", text = NULL, encoding = "UTF-8", ...) {

  # handle NULL text specifically
  if (!missing(text) && is.null(text))
    return(NULL)

  # suppress warnings during parse
  suppressWarnings(parse(file = file, text = text, encoding = encoding, ...))

}

renv_deparse <- function(expr, width.cutoff = 500L, ...) {
  paste(deparse(expr = expr, width.cutoff = width.cutoff, ...), collapse = " ")
}
