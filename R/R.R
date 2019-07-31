
R <- function() {
  bin <- R.home("bin")
  exe <- if (renv_platform_windows()) "R.exe" else "R"
  file.path(bin, exe)
}

r_exec <- function(package, args, label) {

  # ensure R_LIBS is set
  rlibs <- paste(renv_libpaths_all(), collapse = .Platform$path.sep)
  renv_scope_envvars(R_LIBS = rlibs, R_LIBS_USER = "", R_LIBS_SITE = "")

  # ensure Rtools is on the PATH for Windows
  renv_scope_rtools()

  # do the install
  output <- suppressWarnings(system2(R(), args, stdout = TRUE, stderr = TRUE))

  # check for successful install
  status <- attr(output, "status") %||% 0L
  if (!identical(status, 0L))
    r_exec_error(package, output, label)

  output

}

r_exec_error <- function(package, output, label) {

  # installation failed; write output for user
  fmt <- "Error %sing package '%s':"
  header <- sprintf(fmt, label, package)

  lines <- paste(rep("=", nchar(header)), collapse = "")
  output <- c(header, lines, "", output)

  # stop with an error
  message <- sprintf("%s of package '%s' failed", label, package)
  error <- simpleError(message = message)
  error$output <- output
  stop(error)

}

r_cmd_install <- function(package, path, library, ...) {

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)

  args <- c(
    "--vanilla",
    "CMD", "INSTALL", "--preclean",
    r_cmd_install_option(package, "configure.args", TRUE),
    r_cmd_install_option(package, "configure.vars", TRUE),
    r_cmd_install_option(package, "install.opts", FALSE),
    "-l", shQuote(library),
    ...,
    shQuote(path)
  )

  output <- r_exec(package, args, "install")

  installpath <- file.path(library, package)
  if (!file.exists(installpath))
    r_exec_error(package, output, "install")

  installpath

}

r_cmd_build <- function(package, path, ...) {

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  args <- c("--vanilla", "CMD", "build", "--md5", ..., shQuote(path))
  output <- r_exec(package, args, "build")

  pasted <- paste(output, collapse = "\n")
  pattern <- "[*] building .([a-zA-Z0-9_.-]+)."
  matches <- regexec(pattern, pasted)
  text <- regmatches(pasted, matches)

  tarball <- text[[1]][[2]]
  if (!file.exists(tarball))
    r_exec_error(package, output, "build")

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
