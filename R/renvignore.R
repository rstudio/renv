
# given a path within a project, read all relevant ignore files
# and generate a pattern that can be used to filter file results
renv_renvignore_pattern <- function(path = getwd(), root = path) {

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)

  # prepare ignores
  ignores <- stack()

  # push in default ignore entries
  defaults <- getOption("renv.renvignore", default = renv_renvignore_defaults())
  ignores$push(renv_renvignore_parse(defaults, root))

  # read ignore files
  parent <- path
  while (parent != dirname(parent)) {

    # attempt to read either .renvignore or .gitignore
    for (file in c(".renvignore", ".gitignore")) {
      candidate <- file.path(parent, file)
      if (file.exists(candidate)) {
        contents <- readLines(candidate, warn = FALSE)
        ignores$push(renv_renvignore_parse(contents, parent))
        break
      }
    }

    # stop once we've hit the project root
    if (parent == root)
      break

    parent <- dirname(parent)

  }

  # collect patterns read
  patterns <- unlist(ignores$data())

  # clean up empty quotes
  patterns <- gsub("\\Q\\E", "", patterns, fixed = TRUE)

  # join into single pattern for matching
  sprintf("(?:%s)", paste(patterns, collapse = "|"))

}

# reads a .gitignore / .renvignore file, and translates the associated
# entries into PCREs which can be combined and used during directory traversal
renv_renvignore_parse <- function(contents, prefix) {

  # read the ignore entries
  entries <- grep("^\\s*(?:#|\\s*$)", contents, value = TRUE, invert = TRUE)

  # remove trailing whitespace
  entries <- gsub("\\s+$", "", entries)

  # remove trailing slashes
  entries <- gsub("/+$", "", entries)

  # entries without a slash should match in whole tree
  noslash <- grep("/", entries, fixed = TRUE, invert = TRUE)
  entries[noslash] <- paste("**", entries[noslash], sep = "/")

  # save any '**' entries seen
  entries <- gsub("/**",  "\001", entries, fixed = TRUE)
  entries <- gsub("**/",  "\002", entries, fixed = TRUE)

  # transform '*' and '?'
  entries <- gsub("*", "\\E[^/]*\\Q", entries, fixed = TRUE)
  entries <- gsub("?", "\\E[^/]\\Q",  entries, fixed = TRUE)

  # restore '**' entries
  entries <- gsub("\001", "/\\E.*\\Q",      entries, fixed = TRUE)
  entries <- gsub("\002", "\\E(?:.*/)?\\Q", entries, fixed = TRUE)

  # enclose in \\Q \\E to ensure e.g. plain '.' are not treated
  # as regex characters
  entries <- sprintf("\\Q%s\\E$", entries)

  # join entries into a single pattern
  pattern <- sprintf("(?:%s)", paste(entries, collapse = "|"))

  # prepend prefix
  sprintf("^\\Q%s/\\E%s", prefix, pattern)

}

# TODO: allow for e.g. `~/.renvignore`?
renv_renvignore_get <- function(project = NULL) {
  project <- project %||% renv_state$project()

  # read defaults
  defaults <- renv_renvignore_defaults()

  # construct path to '.renvignore'
  path <- file.path(project, ".renvignore")
  if (!renv_file_exists(path))
    return(defaults)

  # return ignores
  ignores <- renv_filebacked_get(path) %||% renv_renvignore_read(path)
  c(ignores, defaults)

}

# TODO: parse things like '**' and so on
renv_renvignore_read <- function(path) {
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")
  matches <- grep("^\\s*(?:#|\\s*$)", contents, value = TRUE, invert = TRUE)
  renv_filebacked_set(path, matches)
}

renv_renvignore_defaults <- function() {
  c("node_modules", "packrat", "revdep", "renv")
}
