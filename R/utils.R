
`%||%` <- function(x, y) {
  if (length(x)) x else y
}

`%&&%` <- function(x, y) {
  if (length(x)) y else NULL
}

`%NA%` <- function(x, y) {
  if (is.na(x)) y else x
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
  if (identical(c(match), 1L))
    path <- paste("~", substring(path, nchar(home) + 1), sep = "/")
  path
}

trimws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

pad_right <- function(text) {

  n <- nchar(text)
  diff <- max(n) - n

  spaces <- map_chr(diff, function(d) {
    paste(rep.int(" ", d), collapse = "")
  })

  paste(text, spaces, sep = "")
}

write_lines <- function(text, con) {
  if (is.null(con)) return(text)
  writeLines(text, con = con, useBytes = TRUE)
}

bind_list <- function(data, name = "Index") {

  filtered <- Filter(NROW, data)
  if (!length(filtered))
    return(NULL)

  rhs <- .mapply(c, filtered, list(use.names = FALSE))
  names(rhs) <- names(filtered[[1]])

  if (name %in% names(rhs)) {
    fmt <- "Name collision: bound list already contains column called '%s'."
    stopf(fmt, name)
  }

  if (is.null(names(data)))
    return(as.data.frame(rhs, stringsAsFactors = FALSE))

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

    if (length(dot) == 2) {
      expr <- dot[[2]]
      return(eval(expr, envir = environment(dot)))
    }

    cond <- dot[[2]]
    expr <- dot[[3]]
    if (eval(cond, envir = environment(dot)))
      return(eval(expr, envir = environment(dot)))
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
  if (renv_debug())
    expr
  else
    tryCatch(expr, error = identity, ...)
}

catchall <- function(expr) {
  if (renv_debug())
    expr
  else
    tryCatch(expr, condition = identity)
}

ask <- function(question) {
  response <- readline(sprintf("%s [Y/n]: ", question))
  tolower(response) %in% c("y", "yes")
}

proceed <- function() {
  ask("Do you want to proceed?")
}

R <- function() {
  file.path(R.home("bin"), "R")
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

