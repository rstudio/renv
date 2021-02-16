
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
        parsed <- renv_renvignore_parse(contents, parent)
        if (length(parsed))
          ignores$push(parsed)
        break
      }
    }

    # stop once we've hit the project root
    if (parent == root)
      break

    parent <- dirname(parent)

  }

  # collect patterns read
  patterns <- ignores$data()
  if (empty(patterns))
    return(list())

  # separate exclusions, exclusions
  include <- unlist(extract(patterns, "include"))
  exclude <- unlist(extract(patterns, "exclude"))

  list(include = include, exclude = exclude)

}

# reads a .gitignore / .renvignore file, and translates the associated
# entries into PCREs which can be combined and used during directory traversal
renv_renvignore_parse <- function(contents, prefix = "") {

  # read the ignore entries
  contents <- grep("^\\s*(?:#|$)", contents, value = TRUE, invert = TRUE)
  if (empty(contents))
    return(list())

  # split into inclusion, exclusion patterns
  negate <- substring(contents, 1L, 1L) == "!"
  exclude <- contents[!negate]
  include <- substring(contents[negate], 2L)

  # parse patterns separately
  list(
    exclude = renv_renvignore_parse_impl(exclude, prefix),
    include = renv_renvignore_parse_impl(include, prefix)
  )

}

renv_renvignore_parse_impl <- function(entries, prefix = "") {

  # check for empty entries list
  if (empty(entries))
    return(character())

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

  # prepend prefix
  entries <- sprintf("^\\Q%s/\\E%s", prefix, entries)

  # remove \\Q\\E
  entries <- gsub("\\Q\\E", "", entries, fixed = TRUE)

  # all done!
  entries

}

renv_renvignore_exec <- function(path, root, children) {

  patterns <- renv_renvignore_pattern(path, root)
  if (empty(patterns) || empty(patterns$exclude))
    return(children)

  # get the entries that need to be excluded
  excludes <- logical(length = length(children))
  for (pattern in patterns$exclude)
    if (nzchar(pattern))
      excludes <- excludes | grepl(pattern, children, perl = TRUE)

  if (length(patterns$include)) {

    # check for entries that should be explicitly included
    # (note that these override any excludes)
    includes <- logical(length = length(children))
    for (pattern in patterns$include)
      if (nzchar(pattern))
        includes <- includes | grepl(pattern, children, perl = TRUE)

    # unset those excludes
    excludes[includes] <- FALSE

  }

  # keep paths not explicitly excluded
  children[!excludes]

}
