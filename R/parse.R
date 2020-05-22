
renv_parse_file <- function(file = "", ...) {
  if (nzchar(file)) {
    text <- readLines(file, warn = FALSE)
    renv_parse_impl(text, srcfile = file, ...)
  }
}

renv_parse_text <- function(text = NULL, ...) {
  if (is.character(text)) {
    renv_parse_impl(text, ...)
  }
}

renv_parse_impl <- function(text, ...) {

  # disable warnings + encoding conversions
  renv_scope_options(
    warn     = 1L,
    encoding = "native.enc"
  )

  # attempt multiple parse methods
  methods <- list(
    renv_parse_impl_asis,
    renv_parse_impl_native,
    renv_parse_impl_utf8
  )

  for (method in methods) {
    parsed <- catch(method(text, ...))
    if (!inherits(parsed, "error"))
      return(parsed)
  }

  # if these all fail, then just try the default
  # parse and let the error propagate
  on.exit(Sys.setlocale(), add = TRUE)
  parse(text = text, ...)

}

renv_parse_impl_asis <- function(text, ...) {
  on.exit(Sys.setlocale(), add = TRUE)
  parse(text = text, ...)
}

renv_parse_impl_native <- function(text, ...) {
  on.exit(Sys.setlocale(), add = TRUE)
  parse(text = enc2native(text), encoding = "unknown", ...)
}

renv_parse_impl_utf8 <- function(text, ...) {
  on.exit(Sys.setlocale(), add = TRUE)
  parse(text = enc2utf8(text), encoding = "UTF-8", ...)
}


renv_deparse <- function(expr, width.cutoff = 500L, ...) {
  paste(deparse(expr = expr, width.cutoff = width.cutoff, ...), collapse = " ")
}
