
renv_parse_file <- function(file = "", ...) {
  if (nzchar(file)) {
    renv_scope_options(warn = -1L)
    text <- readLines(file, warn = FALSE, encoding = "UTF-8")
    renv_parse_impl(text, srcfile = file, ...)
  }
}

renv_parse_text <- function(text = NULL, ...) {
  if (is.character(text)) {
    renv_parse_impl(text, ...)
  }
}

renv_parse_impl <- function(text, ...) {

  # save default encoding
  enc <- Encoding(text)

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

  # attempt with different guessed encodings
  encodings <- c("UTF-8", "unknown")

  for (encoding in encodings) {
    Encoding(text) <- encoding
    for (method in methods) {
      parsed <- catch(method(text, ...))
      if (!inherits(parsed, "error"))
        return(parsed)
    }
  }

  # if these all fail, then just try the default
  # parse and let the error propagate
  defer(Sys.setlocale())
  Encoding(text) <- enc
  parse(text = text, ...)

}

renv_parse_impl_asis <- function(text, ...) {
  defer(Sys.setlocale())
  parse(text = text, ...)
}

renv_parse_impl_native <- function(text, ...) {
  defer(Sys.setlocale())
  parse(text = enc2native(text), encoding = "unknown", ...)
}

renv_parse_impl_utf8 <- function(text, ...) {
  defer(Sys.setlocale())
  parse(text = enc2utf8(text), encoding = "UTF-8", ...)
}

