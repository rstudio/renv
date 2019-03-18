
# a very primitive JSON reader, giving us just enough to read the things
# that the GitHub API produces for us. note that the implementation depends
# on the JSON being 'nicely' formatted, with braces etc. only occurring at
# the starts / ends of lines
renv_json_read <- function(file, text = NULL) {
  text <- text %||% readLines(file, warn = FALSE)
  transformed <- text
  transformed <- gsub("[[{]$", "list(", transformed)
  transformed <- gsub("[]}]$", ")", transformed)
  transformed <- gsub("\":", "\" =", transformed)
  parse(text = paste(transformed, collapse = "\n"))
}
