
renv_cli_exec <- function(clargs = commandArgs(trailingOnly = TRUE)) {
  invisible(renv_cli_exec_impl(clargs))
}

renv_cli_exec_impl <- function(clargs) {

  # check for tool called without arguments, or called with '--help'
  usage <-
    length(clargs) == 0 ||
    clargs[1L] %in% c("help", "--help")

  if (usage)
    return(renv_cli_usage())

  # extract method
  method <- clargs[1L]

  # check request for help on requested method
  help <-
    clargs[2L] %in% c("help", "--help")

  if (help)
    return(renv_cli_help(method))

  # check for known function in renv
  exports <- getNamespaceExports("renv")
  if (!method %in% exports)
    return(renv_cli_unknown(method, exports))

  # begin building call
  args <- list(call("::", as.name("renv"), as.name(method)))

  for (clarg in clargs[-1L]) {

    # convert '--no-<flag>' into a FALSE parameter
    if (grepl("^--no-", clarg)) {
      key <- substring(clarg, 6L)
      args[[key]] <- FALSE
    }

    # convert '--param=value' flags
    else if (grepl("^--[^=]+=", clarg)) {
      index <- regexpr("=", clarg, fixed = TRUE)
      key <- substring(clarg, 3L, index - 1L)
      val <- substring(clarg, index + 1L)
      args[[key]] <- renv_cli_parse(val)
    }

    # convert '--flag' into a TRUE parameter
    else if (grepl("^--", clarg)) {
      key <- substring(clarg, 3L)
      args[[key]] <- TRUE
    }

    # take other parameters as-is
    else {
      splat <- strsplit(clarg, ",", fixed = TRUE)[[1L]]
      args[[length(args) + 1L]] <- renv_cli_parse(splat)
    }

  }

  # invoke method with parsed arguments
  expr <- as.call(args)
  eval(expr = expr, envir = globalenv())

}

renv_cli_usage <- function() {

  usage <- "
Usage: renv [method] [args...]

[method] should be the name of a function exported from renv.
[args...] should be arguments accepted by that function.

Use renv [method] --help for more information about the associated function.

Examples:

  # basic commands
  renv init      # initialize a project
  renv snapshot  # snapshot project library
  renv restore   # restore project library
  renv status    # check project status

  # install a package
  renv install dplyr

  # run a script in an renv project
  renv run path/to/script.R
"

  writeLines(usage, con = stderr())

}

renv_cli_help <- function(method) {
  print(help(method, package = "renv"))
}

renv_cli_unknown <- function(method, exports) {

  # report unknown command
  fmt <- "renv: '%s' is not a known command."
  writef(fmt, method, con = stderr())

  # check for similar commands
  distance <- c(adist(method, exports))
  names(distance) <- exports
  n <- min(distance)
  if (n > 2)
    return(1L)

  candidates <- names(distance)[distance == n]
  fmt <- "did you mean %s?"
  vwritef(fmt, paste(shQuote(candidates), collapse = " or "))
  return(1L)

}

renv_cli_parse <- function(text) {

  expr <- parse(text = text)

  for (i in seq_along(expr))
    if (is.symbol(expr[[i]]))
      expr[[i]] <- as.character(expr[[i]])

  unlist(as.list(expr), recursive = FALSE)

}
