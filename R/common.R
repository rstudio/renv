
catch <- function(expr) {
  tryCatch(
    withCallingHandlers(expr, error = renv_error_capture),
    error = renv_error_tag
  )
}

read <- function(file) {

  # disable warnings in this scope
  op <- options(warn = -1L)
  on.exit(options(op), add = TRUE)

  # read the contents of the file
  contents <- readLines(file, warn = FALSE)
  paste(contents, collapse = "\n")

}
