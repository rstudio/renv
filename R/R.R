
R <- function() {
  bin <- R.home("bin")
  exe <- if (renv_platform_windows()) "R.exe" else "R"
  file.path(bin, exe)
}

r_exec <- function(package, args, label) {

  # pass along environment variables to child process
  keys <- c("R_ENVIRON", "R_ENVIRON_USER", "R_MAKEVARS", "R_MAKEVARS_USER")
  vals <- Sys.getenv(keys, unset = NA)
  envvars <- vals[!is.na(vals)]

  # include R_LIBS
  envvars <- c(
    R_LIBS = paste(renv_libpaths_all(), collapse = .Platform$path.sep),
    envvars
  )

  # generate env as used by system2
  env <- paste(names(envvars), shQuote(envvars), sep = "=")

  # do the install
  output <- suppressWarnings(
    system2(R(), args, stdout = TRUE, stderr = TRUE, env = env)
  )

  # check for successful install
  status <- attr(output, "status") %||% 0L
  if (identical(status, 0L))
    return(output)

  # installation failed; write output for user
  fmt <- "Error %sing package '%s':"
  header <- sprintf(fmt, label, package)

  lines <- paste(rep("=", nchar(header)), collapse = "")
  all <- c(header, lines, "", output)
  vwritef(all, con = stderr())

  # stop with an error
  stopf("%s of package '%s' failed", label, package)

}

r_cmd_install <- function(package, path, library, ...) {

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)

  args <- c(
    "CMD", "INSTALL", "--preclean",
    r_cmd_install_option(package, "configure.args", TRUE),
    r_cmd_install_option(package, "configure.vars", TRUE),
    r_cmd_install_option(package, "install.opts", FALSE),
    "-l", shQuote(library),
    ...,
    shQuote(path)
  )

  r_exec(package, args, "install")
  file.path(library, package)

}

r_cmd_build <- function(package, path, ...) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  args <- c("CMD", "build", "--md5", ..., shQuote(path))
  output <- r_exec(package, args, "build")
  pasted <- paste(output, collapse = "\n")
  pattern <- "[*] building .([a-zA-Z0-9_.-]+)."
  matches <- regexec(pattern, pasted)
  text <- regmatches(pasted, matches)
  tarball <- text[[1]][[2]]
  file.path(getwd(), tarball)
}

r_cmd_install_option <- function(package, option, configure) {

  # read option
  value <- getOption(option)
  if (is.null(value))
    return(NULL)

  # check for named values
  if (package %in% names(value)) {
    value <- value[[package]]
    if (is.null(value))
      return(value)
  }

  # if this is a configure option, format specially
  if (configure) {
    confkey <- sub(".", "-", option, fixed = TRUE)
    confval <- shQuote(paste(value, collapse = " "))
    return(sprintf("--%s=%s", confkey, confval))
  }

  # otherwise, just paste it
  paste(value, collapse = " ")

}
