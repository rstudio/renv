
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

`%NA%` <- function(x, y) {
  if (length(x) && is.na(x)) y else x
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
  length(x) == 0L
}

zlength <- function(x) {
  length(x) != 0L
}

trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x, perl = TRUE)
}

trimws <- function(x) {
  gsub("^\\s+|\\s+$", "", x, perl = TRUE)
}

case <- function(...) {

  dots <- eval(substitute(alist(...)))
  for (i in seq_along(dots)) {

    if (identical(dots[[i]], quote(expr = )))
      next

    dot <- eval(dots[[i]], envir = parent.frame())
    if (!inherits(dot, "formula"))
      return(dot)

    # Silence R CMD check note
    expr <- NULL
    cond <- NULL

    # use delayed assignments below so we can allow return statements to
    # be handled in the lexical scope where they were defined
    if (length(dot) == 2L) {
      do.call(delayedAssign, list("expr", dot[[2L]], eval.env = environment(dot)))
      return(expr)
    }

    do.call(delayedAssign, list("cond", dot[[2L]], eval.env = environment(dot)))
    do.call(delayedAssign, list("expr", dot[[3L]], eval.env = environment(dot)))
    if (cond) return(expr)

  }

}

compose <- function(wrapper, callback) {
  function(...) wrapper(callback(...))
}

catch <- function(expr) {
  tryCatch(
    withCallingHandlers(expr, error = renv_error_capture),
    error = renv_error_tag
  )
}

catchall <- function(expr) {
  tryCatch(
    withCallingHandlers(
      expr = expr,
      error = renv_error_capture,
      warning = renv_error_capture
    ),
    error = renv_error_tag,
    warning = renv_error_tag
  )
}

# nocov start

ask <- function(question, default = FALSE) {

  if (renv_tests_running())
    return(TRUE)

  enabled <- getOption("renv.prompt.enabled", default = TRUE)
  if (!enabled)
    return(default)

  if (!interactive())
    return(default)

  # can't prompt for input when autoloading; code run from `.Rprofile` should
  # not attempt to interact with the user
  # from `?Startup`:
  # "It is not intended that there be interaction with the user during startup
  # code. Attempting to do so can crash the R process."
  # https://github.com/rstudio/renv/issues/1879
  if (autoloading())
    return(default)

  # be verbose in this scope, as we're asking the user for input
  renv_scope_options(renv.verbose = TRUE)

  repeat {

    # solicit user's answer
    selection <- if (default) "[Y/n]" else "[y/N]"
    prompt <- sprintf("%s %s: ", question, selection)
    response <- tryCatch(
      tolower(trimws(readline(prompt))),
      interrupt = identity
    )

    # check for interrupts; treat as abort request
    cancel_if(inherits(response, "interrupt"))

    # use default when no response
    if (!nzchar(response))
      return(default)

    # check for 'yes' responses
    if (response %in% c("y", "yes")) {
      writef("")
      return(TRUE)
    }

    # check for 'no' responses
    if (response %in% c("n", "no")) {
      writef("")
      return(FALSE)
    }

    # ask the user again
    writef("- Unrecognized response: please enter 'y' or 'n', or type Ctrl + C to cancel.")

  }

}

proceed <- function(default = TRUE) {
  ask("Do you want to proceed?", default = default)
}

menu <- function(choices, title, default = 1L) {

  testing <- getOption("renv.menu.choice", integer())
  selected <- if (length(testing)) {
    options(renv.menu.choice = testing[-1L])
    testing[[1L]]
  } else if (testing()) {
    default
  }

  if (!is.null(selected)) {
    title <- paste(title, collapse = "\n")
    body <- paste(sprintf("%i: %s", seq_along(choices), choices), collapse = "\n")
    footer <- sprintf("Selection: %s\n", selected)
    writef(paste(c(title, body, footer), collapse = "\n\n"))
    return(names(choices)[selected])
  }

  if (!interactive()) {
    value <- if (is.numeric(default)) names(choices)[default] else default
    return(value)
  }

  idx <- tryCatch(
    utils::menu(choices, paste(title, collapse = "\n"), graphics = FALSE),
    interrupt = function(cnd) 0L
  )

  if (idx == 0L)
    return("cancel")

  names(choices)[idx]

}

# nocov end

insert <- function(contents,
                   pattern,
                   replacement,
                   anchor = NULL,
                   fixed  = FALSE)
{
  # first, check to see if the pattern matches a line
  index <- grep(pattern, contents, perl = !fixed, fixed = fixed)
  if (length(index)) {
    contents[index] <- replacement
    return(contents)
  }

  # otherwise, check for the anchor, and insert after
  index <- if (!is.null(anchor))
    grep(anchor, contents, perl = !fixed, fixed = fixed)

  if (!length(index))
    return(c(contents, replacement))

  c(
    head(contents, n = index),
    replacement,
    tail(contents, n = -index)
  )
}

deparsed <- function(value, width = 60L) {
  paste(deparse(value, width.cutoff = width), collapse = "\n")
}

read <- function(file) {
  renv_scope_options(warn = -1L)
  contents <- readLines(file, warn = FALSE)
  paste(contents, collapse = "\n")
}

plural <- function(word, n) {
  suffixes <- c("", "s")
  indices <- as.integer(n != 1L) + 1L
  paste0(word, suffixes[indices])
}

nplural <- function(word, n) {
  paste(n, plural(word, n))
}

trunc <- function(text, n = 78) {
  long <- nchar(text) > n
  text[long] <- sprintf("%s <...>", substring(text[long], 1, n - 6))
  text
}

endswith <- function(string, suffix) {
  substring(string, nchar(string) - nchar(suffix) + 1) == suffix
}

# like tools::file_ext, but includes leading '.', and preserves
# '.tar.gz', '.tar.bz' and so on
fileext <- function(path, default = "") {
  indices <- regexpr("[.]((?:tar[.])?[[:alnum:]]+)$", path, perl = TRUE)
  ifelse(indices > -1L, substring(path, indices), default)
}

visited <- function(name, envir) {
  value <- envir[[name]] %||% FALSE
  envir[[name]] <- TRUE
  value
}

zmap <- function(x, f) {
  callback <- function(x) do.call(f, x)
  lapply(x, callback)
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
    defer(sink(NULL))
  }

  withCallingHandlers(
    expr,
    warning               = function(c) invokeRestart("muffleWarning"),
    message               = function(c) invokeRestart("muffleMessage"),
    packageStartupMessage = function(c) invokeRestart("muffleMessage")
  )

}

# NOTE: This function can be used in preference to `as.*()` if you'd like
# to preserve attributes on the incoming object 'x'.
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

keep <- function(x, keys) {
  x[intersect(keys, names(x))]
}

keep_if <- function(x, f) {
  x[f(x)]
}

omit <- function(x, keys) {
  x[setdiff(names(x), keys)]
}

omit_if <- function(x, f) {
  x[!f(x)]
}

invoke <- function(callback, ...) {
  callback(...)
}

resolve <- function(object) {

  while (is.function(object))
    object <- object()

  object

}

dequote <- function(strings) {

  for (quote in c("'", '"')) {

    # find strings matching pattern
    pattern <- paste0(quote, "(.*)", quote)
    matches <- grep(pattern, strings, perl = TRUE)
    if (empty(matches))
      next

    # remove outer quotes
    strings[matches] <- gsub(pattern, "\\1", strings[matches], perl = TRUE)

    # un-escape inner quotes
    pattern <- paste0("\\", quote)
    strings[matches] <- gsub(pattern, quote, strings[matches], fixed = TRUE)
  }

  strings

}

nth <- function(x, i) {
  x[[i]]
}

find <- function(x, f, ...) {
  for (i in seq_along(x))
    if (!is.null(value <- f(x[[i]], ...)))
      return(value)
}

recursing <- function() {

  nf <- sys.nframe()
  if (nf < 2L)
    return(FALSE)

  np <- sys.parent()
  fn <- sys.function(np)
  for (i in seq_len(np - 1L))
    if (identical(fn, sys.function(i)))
      return(TRUE)

  FALSE

}

csort <- function(x, decreasing = FALSE, ...) {
  renv_scope_locale("LC_COLLATE", "C")
  sort(x, decreasing, ...)
}

fsub <- function(pattern, replacement, x, ignore.case = FALSE, useBytes = FALSE) {
  sub(pattern, replacement, x, ignore.case = ignore.case, useBytes = useBytes, fixed = TRUE)
}

rows <- function(data, indices) {

  # convert logical values
  if (is.logical(indices)) {
    if (length(indices) < nrow(data))
      indices <- rep(indices, length.out = nrow(data))
    indices <- which(indices, useNames = FALSE)
  }

  # build output list
  output <- vector("list", length(data))
  for (i in seq_along(data))
    output[[i]] <- .subset2(data, i)[indices]

  # copy relevant attributes
  attrs <- attributes(data)
  attrs[["row.names"]] <- .set_row_names(length(indices))
  attributes(output) <- attrs

  # return new data.frame
  output

}

cols <- function(data, indices) {

  # perform subset
  output <- .subset(data, indices)

  # copy relevant attributes
  attrs <- attributes(data)
  attrs[["names"]] <- attr(output, "names", exact = TRUE)
  attributes(output) <- attrs

  # return output
  output

}

stringify <- function(object, collapse = " ") {

  if (is.symbol(object))
    return(as.character(object))

  paste(
    deparse(object, width.cutoff = 500L),
    collapse = collapse
  )

}

env <- function(...) {
  list2env(list(...), envir = new.env(parent = emptyenv()))
}

env2list <- function(env) {
  as.list.environment(env, all.names = TRUE)
}

chop <- function(x, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE) {
  strsplit(x, split, !perl, perl, useBytes)[[1L]]
}

prof <- function(expr, ...) {

  profile <- tempfile("renv-profile-", fileext = ".Rprof")

  Rprof(profile, ...)
  result <- expr
  Rprof(NULL)
  print(summaryRprof(profile))

  invisible(result)

}

recycle <- function(data) {

  # compute number of columns
  n <- lengths(data, use.names = FALSE)
  nrow <- max(n)

  # start recycling
  for (i in seq_along(data)) {
    if (n[[i]] == 0L) {
      length(data[[i]]) <- nrow
    } else if (n[[i]] != nrow) {
      data[[i]] <- rep.int(data[[i]], nrow / n[[i]])
    }
  }

  data

}

take <- function(data, index = NULL) {
  if (is.null(index)) data else .subset2(data, index)
}

cancel <- function(verbose = TRUE) {

  renv_snapshot_auto_suppress_next()
  if (testing())
    stop("Operation canceled", call. = FALSE)

  if (verbose)
    message("- Operation canceled.")
  invokeRestart("abort")

}

cancel_if <- function(cnd) {
  if (cnd) cancel()
}

rep_named <- function(names, x) {
  values <- rep_len(x, length(names))
  names(values) <- names
  values
}

wait_until <- function(callback, ...) {
  repeat if (callback(...)) return(TRUE)
}

timer <- function(units = "secs") {

  .time <- Sys.time()
  .units <- units

  list(

    now = function() {
      Sys.time()
    },

    elapsed = function() {
      difftime(Sys.time(), .time, units = .units)
    }
  )

}

summon <- function() {
  envir <- do.call(attach, list(what = NULL, name = "renv"))
  renv <- renv_envir_self()
  list2env(as.list(renv), envir = envir)
}

overlay <- function(lhs, rhs) {
  modifyList(as.list(lhs), as.list(rhs))
}

# the 'top' renv function in the call stack
topfun <- function() {

  self <- renv_envir_self()
  frames <- sys.frames()

  for (i in seq_along(frames))
    if (identical(self, parent.env(frames[[i]])))
      return(sys.function(i))

}

warnify <- function(cnd) {
  class(cnd) <- c("warning", "condition")
  warning(cnd)
}

# note: also handles stringy values like 'True'
not <- function(value) {
  if (value) FALSE else TRUE
}

wait <- function(predicate, ...) {
  while (TRUE)
    if (predicate(...))
      break
}
