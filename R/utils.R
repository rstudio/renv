
`%||%` <- function(x, y) {
  if (length(x)) x else y
}

`%NA%` <- function(x, y) {
  if (is.na(x)) y else x
}

`%NULL%` <- function(x, y) {
  if (is.null(x)) y else x
}

`%&&%` <- function(x, y) {
  if (length(x)) y
}


lines <- function(...) {
  paste(..., sep = "\n")
}

is_named <- function(x) {
  nm <- names(x)
  !is.null(nm) && all(nzchar(nm))
}

named <- function(object, names = object) {
  names(object) <- names
  object
}

empty <- function(x) {
  length(x) == 0
}

aliased_path <- function(path) {
  home <- path.expand("~/")
  match <- regexpr(home, path, fixed = TRUE, useBytes = TRUE)
  path[match == 1] <- paste("~", substring(path[match == 1], nchar(home) + 1), sep = "/")
  path
}

trimws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

bind_list <- function(data, names = NULL, name = "Index") {

  filtered <- Filter(NROW, data)
  if (!length(filtered))
    return(NULL)

  rhs <- .mapply(c, filtered, list(use.names = FALSE))
  names(rhs) <- names(filtered[[1]])

  if (name %in% names(rhs)) {
    fmt <- "name collision: bound list already contains column called '%s'"
    stopf(fmt, name)
  }

  if (is.null(names(data))) {
    names(rhs) <- names(rhs) %||% names
    return(as.data.frame(rhs, stringsAsFactors = FALSE))
  }

  lhs <- list()
  lhs[[name]] <- rep.int(names(data), times = map_dbl(filtered, NROW))

  cbind(
    as.data.frame(lhs, stringsAsFactors = FALSE),
    as.data.frame(rhs, stringsAsFactors = FALSE)
  )

}

case <- function(...) {

  dots <- list(...)
  for (dot in dots) {

    if (!inherits(dot, "formula"))
      return(dot)

    else if (length(dot) == 2) {
      expr <- dot[[2]]
      return(eval(expr, envir = environment(dot)))
    }

    else {

      cond <- dot[[2]]
      expr <- dot[[3]]
      if (eval(cond, envir = environment(dot)))
        return(eval(expr, envir = environment(dot)))

    }
  }

  NULL

}

version_compare <- function(lhs, rhs) {

  lhs <- unclass(numeric_version(lhs))[[1]]
  rhs <- unclass(numeric_version(rhs))[[1]]

  n <- max(length(lhs), length(rhs))
  for (i in seq_len(n)) {
    l <- lhs[i] %NA% 0; r <- rhs[i] %NA% 0
    if (l < r) return(-1)
    if (l > r) return(+1)
  }

  0

}

catch <- function(expr, ...) {
  if (renv_debugging())
    expr
  else
    tryCatch(expr, error = identity, ...)
}

catchall <- function(expr) {
  if (renv_debugging())
    expr
  else
    tryCatch(expr, condition = identity)
}

ask <- function(question) {
  response <- readline(sprintf("%s [y/N]: ", question))
  tolower(response) %in% c("y", "yes")
}

proceed <- function() {
  ask("Do you want to proceed?")
}

inject <- function(contents,
                   pattern,
                   replacement,
                   anchor)
{
  # first, check to see if the pattern matches a line
  index <- grep(pattern, contents)
  if (length(index)) {
    contents[index] <- replacement
    return(contents)
  }

  # otherwise, check for the anchor, and insert after
  index <- grep(anchor, contents)
  if (length(index)) {
    contents <- c(
      head(contents, n = index),
      replacement,
      tail(contents, n = -index)
    )
    return(contents)
  }

  stopf("edit failed")
}

env <- function(...) {
  list2env(list(...), envir = new.env(parent = emptyenv()))
}

deparsed <- function(value, width = 60L) {
  paste(deparse(value, width.cutoff = width), collapse = "\n")
}

read <- function(file) {
  contents <- readLines(file, warn = FALSE)
  paste(contents, collapse = "\n")
}

plural <- function(word, n) {
  if (n == 1) word else paste(word, "s", sep = "")
}

trunc <- function(text, n = 78) {
  long <- nchar(text) > n
  text[long] <- sprintf("%s <...>", substring(text[long], 1, n - 6))
  text
}

startswith <- function(string, prefix) {
  substring(string, 1, nchar(prefix)) == prefix
}

endswith <- function(string, suffix) {
  substring(string, nchar(string) - nchar(suffix) + 1) == suffix
}

# like tools::file_ext, but includes leading '.', and preserves
# '.tar.gz', '.tar.bz' and so on
fileext <- function(path, default = "") {
  indices <- regexpr("[.]((?:tar[.])?[[:alnum:]]+)$", path)
  ifelse(indices > -1L, substring(path, indices), default)
}

flip <- function(vector) {
  named(names(vector), vector)
}

git <- function() {

  gitpath <- Sys.which("git")
  if (!nzchar(gitpath))
    stop("failed to find git executable on the PATH")

  gitpath

}

visited <- function(name, envir) {

  if (exists(name, envir = envir))
    return(TRUE)

  envir[[name]] <- TRUE
  FALSE

}

rowapply <- function(X, FUN, ...) {
  lapply(seq_len(NROW(X)), function(I) {
    FUN(X[I, ], ...)
  })
}
