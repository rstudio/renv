
# given a path within a project, read all relevant ignore files
# and generate a pattern that can be used to filter file results
renv_renvignore_pattern <- function(path = getwd(), root = path) {

  if (is.null(root))
    return(NULL)

  stopifnot(
    renv_path_absolute(path),
    renv_path_absolute(root)
  )

  # prepare ignores
  ignores <- stack()

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
  if (empty(patterns))
    return(patterns)

  # join into single pattern for matching
  sprintf("(?:%s)", paste(patterns, collapse = "|"))

}

# reads a .gitignore / .renvignore file, and translates the associated
# entries into PCREs which can be combined and used during directory traversal
renv_renvignore_parse <- function(contents, prefix = "") {

  # read the ignore entries
  entries <- grep("^\\s*(?:#|\\s*$)", contents, value = TRUE, invert = TRUE)

  # remove trailing whitespace
  entries <- gsub("\\s+$", "", entries)

  # remove trailing slashes
  entries <- gsub("/+$", "", entries)

  # entries without a slash should match in whole tree
  noslash <- grep("/", entries, fixed = TRUE, invert = TRUE)
  entries[noslash] <- paste("**", entries[noslash], sep = "/")

  # remove a leading slash (avoid double-slashing)
  entries <- gsub("^/+", "", entries)

  # save any '**' entries seen
  entries <- gsub("**/",  "\001", entries, fixed = TRUE)
  entries <- gsub("/**",  "\002", entries, fixed = TRUE)

  # transform '*' and '?'
  entries <- gsub("*", "\\E[^/]*\\Q", entries, fixed = TRUE)
  entries <- gsub("?", "\\E[^/]\\Q",  entries, fixed = TRUE)

  # restore '**' entries
  entries <- gsub("\001", "\\E(?:.*/)?\\Q", entries, fixed = TRUE)
  entries <- gsub("\002", "/\\E.*\\Q",      entries, fixed = TRUE)

  # enclose in \\Q \\E to ensure e.g. plain '.' are not treated
  # as regex characters
  entries <- sprintf("\\Q%s\\E$", entries)

  # join entries into a single pattern
  pattern <- sprintf("(?:%s)", paste(entries, collapse = "|"))

  # prepend prefix
  pattern <- sprintf("^\\Q%s/\\E%s", prefix, pattern)

  # remove \\Q\\E
  gsub("\\Q\\E", "", pattern, fixed = TRUE)

}

renv_renvignore_exec <- function(path, root, children) {

  pattern <- renv_renvignore_pattern(path, root)
  if (empty(pattern))
    return(children)

  grep(pattern, children, perl = TRUE, invert = TRUE, value = TRUE)

}
