
renv_error_handler <- function(...) {

  calls <- head(sys.calls(), n = -1L)
  formatted <- map_chr(calls, function(call) {
    trunc(paste(format(call), collapse = " "))
  })

  header <- "Traceback (most recent calls first):"
  numbers <- format(seq_along(formatted))
  text <- sprintf("  %s: %s", rev(numbers), rev(formatted))

  contents <- paste(c(header, text), collapse = "\n")
  writeLines(contents, con = stderr())

}
