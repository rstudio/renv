
R <- function() {
  bin <- R.home("bin")
  exe <- if (renv_platform_windows()) "R.exe" else "R"
  file.path(bin, exe)
}

r_exec <- function(package, args, label) {

  # ensure R_LIBS is set in case packages are needed during
  # R CMD <...> actions
  rlibs <- paste(renv_libpaths_all(), collapse = .Platform$path.sep)
  env <- paste("R_LIBS", shQuote(rlibs), sep = "=")

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
  vwritef(paste(all, collapse = "\n"), con = stderr())

  # stop with an error
  stopf("%s of package '%s' failed", label, package)

}

r_cmd_install <- function(package, path, library) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)
  args <- c("CMD", "INSTALL", "--preclean", "-l", shQuote(library), shQuote(path))
  r_exec(package, args, "install")
  file.path(library, package)
}

r_cmd_build <- function(package, path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  args <- c("CMD", "build", "--md5", shQuote(path))
  output <- r_exec(package, args, "build")
  pasted <- paste(output, collapse = "\n")
  pattern <- "[*] building .([a-zA-Z0-9_.-]+)."
  matches <- regexec(pattern, pasted)
  text <- regmatches(pasted, matches)
  tarball <- text[[1]][[2]]
  file.path(getwd(), tarball)
}
