
R <- function() {
  bin <- R.home("bin")
  exe <- if (renv_platform_windows()) "R.exe" else "R"
  file.path(bin, exe)
}

r_exec <- function(args, ...) {

  # ensure R_LIBS is set
  #
  # TODO: on Windows, if R_LIBS_USER is empty or unset, R will automatically
  # set R_LIBS_USER to the user-specific package directory. do we want that
  # behavior here? if not, we have to set it to a non-existent path
  rlibs <- paste(renv_libpaths_all(), collapse = .Platform$path.sep)
  renv_scope_envvars(R_LIBS = rlibs, R_LIBS_USER = "", R_LIBS_SITE = "")

  # ensure Rtools is on the PATH for Windows
  renv_scope_rtools()

  # invoke r
  suppressWarnings(system2(R(), args, ...))

}

r_exec_error <- function(package, output, label, extra) {

  # installation failed; write output for user
  fmt <- "Error %sing package '%s':"
  header <- sprintf(fmt, label, package)

  lines <- paste(rep("=", nchar(header)), collapse = "")
  all <- c(header, lines, "", output)

  # try to add diagnostic information if possible
  diagnostics <- r_exec_error_diagnostics(package, output)
  if (!empty(diagnostics)) {
    size <- min(getOption("width"), 78L)
    dividers <- paste(rep.int("-", size), collapse = "")
    all <- c(all, paste(dividers, diagnostics, collapse = "\n\n"))
  }

  # normalize 'extra'
  extra <- if (is.integer(extra))
    paste("error code", extra)
  else
    paste(renv_path_pretty(extra), "does not exist")

  # stop with an error
  message <- sprintf("%s of package '%s' failed [%s]", label, package, extra)
  error <- simpleError(message = message)
  error$output <- all
  stop(error)

}

r_exec_error_diagnostics_fortran_library <- function() {

  checker <- function(output) {
    pattern <- "library not found for -l(quadmath|gfortran|fortran)"
    idx <- grep(pattern, output, ignore.case = TRUE)
    if (length(idx))
      return(unique(output[idx]))
  }

  suggestion <- "
R was unable to find one or more FORTRAN libraries during compilation.
This often implies that the FORTRAN compiler has not been properly configured.
Please see https://stackoverflow.com/q/35999874 for more information.
"

  list(
    checker = checker,
    suggestion = suggestion
  )

}

r_exec_error_diagnostics_fortran_binary <- function() {

  checker <- function(output) {
    pattern <- "gfortran: no such file or directory"
    idx <- grep(pattern, output, ignore.case = TRUE)
    if (length(idx))
      return(unique(output[idx]))
  }

  suggestion <- "
R was unable to find the gfortran binary.
gfortran is required for the compilation of FORTRAN source files.
Please check that gfortran is installed and available on the PATH.
Please see https://stackoverflow.com/q/35999874 for more information.
"

  list(
    checker = checker,
    suggestion = suggestion
  )

}

r_exec_error_diagnostics_openmp <- function() {

  checker <- function(output) {
    pattern <- "unsupported option '-fopenmp'"
    idx <- grep(pattern, output, fixed = TRUE)
    if (length(idx))
      return(unique(output[idx]))
  }

  suggestion <- "
R is currently configured to use a compiler that does not have OpenMP support.
You may need to disable OpenMP, or update your compiler toolchain.
Please see https://support.bioconductor.org/p/119536/ for a related discussion.
"

  list(
    checker = checker,
    suggestion = suggestion
  )

}

r_exec_error_diagnostics <- function(package, output) {

  diagnostics <- list(
    r_exec_error_diagnostics_fortran_library(),
    r_exec_error_diagnostics_fortran_binary(),
    r_exec_error_diagnostics_openmp()
  )

  suggestions <- uapply(diagnostics, function(diagnostic) {

    check <- catch(diagnostic$checker(output))
    if (!is.character(check))
      return()

    suggestion <- diagnostics$suggestion
    reasons <- paste("-", shQuote(check), collapse = "\n")
    paste(diagnostic$suggestion, "Reason(s):", reasons, sep = "\n")

  })

  as.character(suggestions)

}

# install package called 'package' located at path 'path'
r_cmd_install <- function(package, path, ...) {

  # normalize path to package
  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)

  # resolve default library path
  library <- renv_libpaths_default()

  # validate that we have command line tools installed and
  # available for e.g. macOS
  if (renv_platform_macos() && renv_package_type(path) == "source")
    renv_xcode_check()

  # perform platform-specific preinstall checks
  renv_scope_install()

  # perform the install
  args <- c(
    "--vanilla",
    "CMD", "INSTALL", "--preclean", "--no-multiarch",
    r_cmd_install_option(package, "configure.args", TRUE),
    r_cmd_install_option(package, "configure.vars", TRUE),
    r_cmd_install_option(package, "install.opts", FALSE),
    ...,
    shQuote(path)
  )

  if (config$install.verbose()) {

    status <- r_exec(args, stdout = "", stderr = "")
    if (!identical(status, 0L))
      stopf("install of package '%s' failed", package)

    installpath <- file.path(library, package)
    if (!file.exists(installpath)) {
      fmt <- "install of package '%s' failed: %s does not exist"
      stopf(fmt, package, renv_path_pretty(installpath))
    }

    installpath

  } else {

    output <- r_exec(args, stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status") %||% 0L
    if (!identical(status, 0L))
      r_exec_error(package, output, "install", status)

    installpath <- file.path(library, package)
    if (!file.exists(installpath))
      r_exec_error(package, output, "install", installpath)

    installpath

  }


}

r_cmd_build <- function(package, path, ...) {

  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)
  args <- c("--vanilla", "CMD", "build", "--md5", ..., shQuote(path))

  output <- r_exec(args, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status") %||% 0L
  if (!identical(status, 0L))
    r_exec_error(package, output, "build", status)

  pasted <- paste(output, collapse = "\n")
  pattern <- "[*] building .([a-zA-Z0-9_.-]+)."
  matches <- regexec(pattern, pasted)
  text <- regmatches(pasted, matches)

  tarball <- text[[1]][[2]]
  if (!file.exists(tarball))
    r_exec_error(package, output, "build", tarball)

  file.path(getwd(), tarball)

}

r_cmd_install_option <- function(package, option, configure) {

  # read option -- first, check for package-specific option, then
  # fall back to 'global' option
  value <-
    getOption(paste(option, package, sep = ".")) %||%
    getOption(option)

  # check INSTALL_opts as well (in case that's used instead of install.opts)
  if (is.null(value) && identical(option, "install.opts")) {
    value <-
      getOption(paste("INSTALL_opts", package, sep = ".")) %||%
      getOption("INSTALL_opts")
  }

  if (is.null(value))
    return(NULL)

  # if the value is named, treat it as a list,
  # mapping package names to their configure arguments
  if (!is.null(names(value)))
    value <- as.list(value)

  # check for named values
  if (!is.null(names(value))) {
    value <- value[[package]]
    if (is.null(value))
      return(NULL)
  }

  # if this is a configure option, format specially
  if (configure) {
    confkey <- sub(".", "-", option, fixed = TRUE)
    confval <- if (!is.null(names(value)))
      shQuote(paste(names(value), value, sep = "=", collapse = " "))
    else
      shQuote(paste(value, collapse = " "))
    return(sprintf("--%s=%s", confkey, confval))
  }

  # otherwise, just paste it
  paste(value, collapse = " ")

}
