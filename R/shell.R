
renv_shell_quote <- function(x) {
  if (length(x))
    shQuote(x)
}

renv_shell_path <- function(x) {
  if (length(x))
    shQuote(path.expand(x))
}
