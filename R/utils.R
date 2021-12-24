
`%>%` <- function(...) {

  dots <- eval(substitute(alist(...)))
  if (length(dots) != 2L)
    stopf("`%>%` called with invalid number of arguments")

  lhs <- dots[[1L]]; rhs <- dots[[2L]]
  if (!is.call(rhs))
    stopf("right-hand side of rhs is not a call")

  data <- c(rhs[[1L]], lhs, as.list(rhs[-1L]))
  call <- as.call(data)

  nm <- names(rhs)
  if (length(nm))
    names(call) <- c("", "", nm[-1L])

  eval(call, envir = parent.frame())

}

`%""%` <- function(x, y) {
  if (length(x) && nzchar(x)) x else y
}

`%NA%` <- function(x, y) {
  if (length(x) && is.na(x)) y else x
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

  home <-
    Sys.getenv("HOME") %""%
    Sys.getenv("R_USER")

  if (!nzchar(home))
    return(path)

  home <- gsub("\\", "/", home, fixed = TRUE)
  path <- gsub("\\", "/", path, fixed = TRUE)

  match <- regexpr(home, path, fixed = TRUE, useBytes = TRUE)
  path[match == 1] <- file.path("~", substring(path[match == 1], nchar(home) + 2L))

  path

}

trimws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

case <- function(...) {

  dots <- eval(substitute(alist(...)))
  for (i in seq_along(dots)) {

    if (identical(dots[[i]], quote(expr = )))
      next

    dot <- eval(dots[[i]], envir = parent.frame())
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

catch <- function(expr) {
  tryCatch(
    withCallingHandlers(expr, error = renv_error_capture),
    error = renv_error_tag
  )
}

catchall <- function(expr) {
  tryCatch(
    withCallingHandlers(expr, condition = renv_error_capture),
    condition = renv_error_tag
  )
}

# nocov start

ask <- function(question, default = FALSE) {

  if (renv_tests_running())
    return(TRUE)

  if (!interactive())
    return(default)

  # TODO: presumedly we don't want to prompt in the autoloader
  # because it might cause issues in RStudio?
  initializing <- getOption("renv.autoloader.running")
  if (identical(initializing, TRUE))
    return(default)

  selection <- if (default) "[Y/n]" else "[y/N]"
  prompt <- sprintf("%s %s: ", question, selection)
  response <- tolower(trimws(readline(prompt)))
  if (!nzchar(response))
    return(default)

  substring(response, 1L, 1L) == "y"

}

proceed <- function(default = FALSE) {
  ask("Do you want to proceed?", default = default)
}

# nocov end

inject <- function(contents,
                   pattern,
                   replacement,
                   anchor = NULL)
{
  # first, check to see if the pattern matches a line
  index <- grep(pattern, contents)
  if (length(index)) {
    contents[index] <- replacement
    return(contents)
  }

  # otherwise, check for the anchor, and insert after
  index <- if (!is.null(anchor)) grep(anchor, contents)
  if (length(index)) {
    contents <- c(
      head(contents, n = index),
      replacement,
      tail(contents, n = -index)
    )
    return(contents)
  }

  # otherwise, just append the new line
  c(contents, replacement)
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
    FUN(X[I, , drop = FALSE], ...)
  })
}

comspec <- function() {
  Sys.getenv("COMSPEC", unset = Sys.which("cmd.exe"))
}

nullfile <- function() {
  if (renv_platform_windows()) "NUL" else "/dev/null"
}

quietly <- function(expr, sink = TRUE) {

  if (sink) {
    sink(file = nullfile())
    on.exit(sink(NULL), add = TRUE)
  }

  withCallingHandlers(
    expr,
    warning               = function(c) invokeRestart("muffleWarning"),
    message               = function(c) invokeRestart("muffleMessage"),
    packageStartupMessage = function(c) invokeRestart("muffleMessage")
  )

}

convert <- function(x, type) {
  storage.mode(x) <- type
  x
}

remap <- function(x, map) {

  # TODO: use match?
  remapped <- x
  enumerate(map, function(key, val) {
    remapped[remapped == key] <<- val
  })
  remapped

}

header <- function(label,
                   prefix = "#",
                   suffix = "=",
                   n = 38L)
{
  n <- n - nchar(label) - nchar(prefix) - 2L
  if (n <= 0)
    return(paste(prefix, label))

  tail <- paste(rep.int(suffix, n), collapse = "")
  paste(prefix, label, tail)

}

keep <- function(x, keys) {
  x[intersect(keys, names(x))]
}

drop <- function(x, keys) {
  x[setdiff(names(x), keys)]
}

invoke <- function(f, ...) {
  f(...)
}

dequote <- function(strings) {

  for (quote in c("'", '"')) {

    # find strings matching pattern
    pattern <- paste0(quote, "(.*)", quote)
    matches <- grep(pattern, strings)
    if (empty(matches))
      next

    # remove outer quotes
    strings[matches] <- gsub(pattern, "\\1", strings[matches])

    # un-escape inner quotes
    pattern <- paste0("\\", quote)
    strings[matches] <- gsub(pattern, quote, strings[matches], fixed = TRUE)
  }

  strings

}

memoize <- function(key, expr, envir) {
  value <- envir[[key]] %||% expr
  envir[[key]] <- value
  value
}

nth <- function(x, i) {
  x[[i]]
}

heredoc <- function(text) {

  # remove leading, trailing whitespace
  trimmed <- gsub("^\\s*\\n|\\n\\s*$", "", text)

  # split into lines
  lines <- strsplit(trimmed, "\n", fixed = TRUE)[[1L]]

  # compute common indent
  indent <- regexpr("[^[:space:]]", lines)
  common <- min(setdiff(indent, -1L))
  paste(substring(lines, common), collapse = "\n")

}

find <- function(x, f, ...) {
  for (i in seq_along(x))
    if (!is.null(value <- f(x[[i]], ...)))
      return(value)
}

recursing <- function() {

  nf <- sys.nframe()
  if (nf < 2)
    return(FALSE)

  np <- sys.parent()
  fn <- sys.function(np)
  for (i in seq_len(np - 1L))
    if (identical(fn, sys.function(i)))
      return(TRUE)

  FALSE

}

code <- function(x) {
  paste(deparse(substitute(x)), collapse = "\n")
}

shcode <- function(x) {
  shQuote(paste(deparse(substitute(x)), collapse = "\n"))
}

csort <- function(x, decreasing = FALSE, ...) {
  renv_scope_locale("LC_COLLATE", "C")
  sort(x, decreasing, ...)
}
