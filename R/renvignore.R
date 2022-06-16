
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

  # separate exclusions, exclusions
  include <- unlist(extract(patterns, "include"))
  exclude <- unlist(extract(patterns, "exclude"))

  # allow for inclusion / exclusion via option
  # (primarily intended for internal use with packrat)
  include <- c(include, renv_renvignore_pattern_extra("include", root))
  exclude <- c(exclude, renv_renvignore_pattern_extra("exclude", root))

  # ignore hidden directories by default
  exclude <- c("/[.][^/]*/$", exclude)

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

  # For include rules, if we're explicitly including a file within
  # a sub-directory, then we need to force all parent directories
  # to also be included. In other words, a rule like:
  #
  #    !a/b/c
  #
  # needs to be implicitly treated like
  #
  #    !/a
  #    !/a/b
  #    !/a/b/c
  #
  # so we perform that transformation here.
  #
  # Note that this isn't perfect; for example, with the .gitignore file
  #
  #    dir
  #    !dir/matched
  #
  # The exclusion of 'dir' will take precedence, and dir/matched won't
  # get a chance to apply.
  include <- sort(unique(unlist(map(include, function(rule) {
    idx <- gregexpr("(?:/|$)", rule, perl = TRUE)[[1L]]
    gsub("^/*", "/", substring(rule, 1L, idx))
  }))))

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

  # entries without a slash (other than a trailing one) should match in tree
  noslash <- grep("/", gsub("/*$", "", entries), fixed = TRUE, invert = TRUE)
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

  # if we don't have a trailing slash, then we can match both files and dirs
  noslash <- grep("/$", entries, invert = TRUE)
  entries[noslash] <- paste0(entries[noslash], "\\E(?:/)?\\Q")

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

  # the root directory is always included
  if (identical(root, children))
    return(FALSE)

  # compute exclusion patterns
  patterns <- renv_renvignore_pattern(path, root)

  # if we have no patterns, then we're not excluding anything
  if (empty(patterns) || empty(patterns$exclude))
    return(logical(length(children)))

  # append slashes to files which are directories
  info <- renv_file_info(children)
  dirs <- info$isdir %in% TRUE
  children[dirs] <- paste0(children[dirs], "/")

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

  # return vector of excludes
  excludes

}

renv_renvignore_pattern_extra <- function(key, root) {

  # check for value from option
  optname <- paste("renv.renvignore", key, sep = ".")
  patterns <- getOption(optname)
  if (is.null(patterns))
    return(NULL)

  # should we use the pattern as-is?
  asis <- attr(patterns, "asis", exact = TRUE)
  if (identical(asis, TRUE))
    return(patterns)

  # otherwise, process it as an .renvignore-style ignore
  root <- attr(patterns, "root", exact = TRUE) %||% root
  patterns <- renv_renvignore_parse(patterns, root)
  patterns[[key]]

}
