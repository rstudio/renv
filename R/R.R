
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
  all <- c(header, lines, "", output)

  # try to add diagnostic information if possible
  diagnostics <- r_exec_error_diagnostics(package, output)
  if (!empty(diagnostics)) {
    size <- min(getOption("width"), 78L)
    dividers <- paste(rep.int("-", size), collapse = "")
    all <- c(all, paste(dividers, diagnostics, collapse = "\n\n"))
  }

  # stop with an error
  message <- sprintf("%s of package '%s' failed", label, package)
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

r_cmd_install <- function(package, path, library, ...) {

  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)

  # prefer using a short path name for the library on Windows,
  # to help avoid issues caused by overly-long paths
  library <- if (renv_platform_windows())
    utils::shortPathName(library)
  else
    renv_path_normalize(library, winslash = "/", mustWork = TRUE)

  # validate that we have command line tools installed and
  # available for e.g. macOS
  if (renv_platform_macos() && renv_package_type(path) == "source")
    renv_xcode_check()

  renv_scope_install()

  args <- c(
    "--vanilla",
    "CMD", "INSTALL", "--preclean", "--no-multiarch",
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

  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)
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
