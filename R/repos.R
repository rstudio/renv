
renv_repos_encode <- function(x) {
  if (length(x) == 1)
    paste(names(x), as.character(x), sep = "=")
  else
    paste(sprintf("\n\t%s=%s", names(x), as.character(x)), collapse = "")
}

renv_repos_decode <- function(x) {
  parts <- strsplit(trimws(x), "(?:,|\\s)+")[[1]]
  idx <- regexpr("=", parts, fixed = TRUE)
  keys <- substring(parts, 1, idx - 1)
  vals <- substring(parts, idx + 1)
  named(trimws(vals), trimws(keys))
}
