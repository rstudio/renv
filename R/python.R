
renv_python <- function(python = NULL) {

  python <- python %||% settings$python()
  if (is.null(python))
    return(NULL)

  # TRUE implies use of a local virtual environment
  if (identical(python, TRUE))
    return(renv_python_local_binary())

  # validate we have a string
  if (!is.character(python)) {
    warning("invalid python setting detected")
    return(NULL)
  }

  path <- renv_python_resolve(python)
  if (!file.exists(path)) {
    fmt <- "requested Python path '%s' does not exist"
    warningf(fmt, aliased_path(path))
    return(NULL)
  }

  path

}

# figure out the appropriate path for the Python executable
# given a virtual environment, conda environment, direct
# path to Python, and so on
renv_python_resolve <- function(python) {

  # if we have slashes in the name, assume this
  # is the full path to a Python virtualenv or similar
  if (!grepl("[/\\\\]", python)) {
    root <- Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
    python <- file.path(root, python)
  }

  renv_python_exe(python)

}


renv_python_local_init <- function() {

  path <- file.path(renv_project(), "renv/python/r-reticulate")
  if (file.exists(path))
    return(path)

  vprintf("* Creating local Python virtual environment ... ", appendLF = FALSE)
  renv_python_virtualenv_create(path)
  vwritef("Done!")
  return(path)

}

renv_python_local_binary <- function() {
  path <- renv_python_local_init()
  renv_python_exe(path)
}

renv_python_exe <- function(path) {

  if (renv_python_is_virtualenv(path)) {
    if (renv_platform_windows())
      file.path(path, "Scripts/python.exe")
    else
      file.path(path, "bin/python")
  } else {
    # TODO: conda?
    path
  }

}

renv_python_active_binary <- function() {

  if ("reticulate" %in% loadedNamespaces() &&
      reticulate::py_available())
  {
    config <- reticulate::py_config()
    return(config$python)
  }

  python <- Sys.getenv("RETICULATE_PYTHON")
  if (file.exists(python))
    return(python)

  if (requireNamespace("reticulate", quietly = TRUE)) {
    config <- reticulate::py_discover_config()
    return(config$python)
  }

  Sys.which("python")

}

renv_python_version <- function(python) {
  python <- normalizePath(python)
  cmd <- paste(shQuote(python), "--version 2>&1")
  output <- catch(system(cmd, intern = TRUE))
  space <- regexpr(" ", output, fixed = TRUE)
  substring(output, space + 1)
}

renv_python_snapshot <- function(project) {

  python <- renv_python()
  if (is.null(python) || !renv_python_is_virtualenv(python))
    return(NULL)

  renv_python_pip_freeze(project, python)

}

renv_python_restore <- function(project) {

  python <- renv_python()
  if (is.null(python) || !renv_python_is_virtualenv(python))
    return(NULL)

  renv_python_pip_restore(project, python)

}

renv_python_pip_freeze <- function(project, python) {
  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "requirements.txt")
  before <- character()
  if (file.exists(path))
    before <- readLines(path, warn = FALSE)

  suffix <- "-m pip freeze 2> /dev/null"
  python <- normalizePath(python)
  command <- paste(shQuote(python), suffix)
  after <- system(command, intern = TRUE)

  if (setequal(before, after))
    return(FALSE)

  writeLines(after, con = path)
  vwritef("* Wrote Python packages to '%s'.", aliased_path(path))
  return(TRUE)
}

renv_python_pip_restore <- function(project, python) {
  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "requirements.txt")
  before <- character()
  if (file.exists(path))
    before <- readLines(path, warn = FALSE)

  suffix <- "-m pip freeze 2> /dev/null"
  python <- normalizePath(python)
  command <- paste(shQuote(python), suffix)
  after <- system(command, intern = TRUE)

  if (setequal(before, after))
    return(FALSE)

  diff <- setdiff(before, after)
  file <- tempfile("renv-requirements-", fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  writeLines(diff, con = file)
  suffix <- paste("-m pip install --upgrade -r", shQuote(file))
  command <- paste(shQuote(python), suffix)
  system(command)

  path <- aliased_path(file.path(project, "requirements.txt"))
  vwritef("* Restored Python packages from '%s'.", aliased_path(path))
}

renv_python_virtualenv_create <- function(path) {
  ensure_parent_directory(path)
  python <- renv_python_active_binary()
  version <- renv_python_version(python)
  module <- if (numeric_version(version) > "3.2") "venv" else "virtualenv"
  fmt <- "%s -m %s %s 2>&1"
  python <- normalizePath(python)
  cmd <- sprintf(fmt, shQuote(python), module, shQuote(path))
  output <- system(cmd, intern = TRUE)
  status <- attr(output, "status") %||% 0L
  if (status != 0L || !file.exists(path)) {
    msg <- c("failed to create virtual environment", output)
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }
  file.exists(path)
}

renv_python_is_virtualenv <- function(python) {

  info <- file.info(python, extra_cols = FALSE)
  if (identical(info$isdir, FALSE))
    python <- dirname(python)

  files <- c("pyvenv.cfg", ".Python")
  for (root in c(python, dirname(python))) {
    paths <- file.path(root, files)
    if (any(file.exists(paths)))
      return(TRUE)
  }

  FALSE
}
