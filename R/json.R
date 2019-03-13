
# a very primitive JSON reader, giving us just enough to read the things
# that the GitHub API produces for us
renv_json_read <- function(file, text = NULL) {
  text <- text %||% readLines(file, warn = FALSE)
  transformed <- text
  transformed <- gsub("\\{$", "list(", transformed)
  transformed <- gsub("\\}$", ")", transformed)
  transformed <- gsub("\":", "\" =", transformed)
  parse(text = paste(transformed, collapse = "\n"))
}
