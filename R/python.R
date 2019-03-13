
renv_python <- function() {

  python <- settings$python()
  if (is.null(python))
    return(NULL)

  if (identical(python, TRUE))
    return(renv_python_local_binary())

  python

}

renv_python_local_binary <- function() {

  path <- file.path(renv_project(), "renv/r-reticulate")
  if (!file.exists(path))
    renv_python_virtualenv_create(path)

  if (renv_platform_windows())
    file.path(path, "Scripts/python.exe")
  else
    file.path(path, "bin/python")

}

renv_python_active_binary <- function() {

  if ("reticulate" %in% loadedNamespaces() &&
      reticulate::py_available())
  {
    config <- reticulate::py_config()
    return(config$python)
  }

  python <- Sys.getenv("RETICULATE_PYTHON", unset = NA)
  if (!is.na(python))
    return(python)

  if (requireNamespace("reticulate", quietly = TRUE)) {
    config <- reticulate::py_discover_config()
    return(config$python)
  }

  Sys.which("python")

}

renv_python_version <- function(python) {
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
  command <- paste(shQuote(python), suffix)
  after <- system(command, intern = TRUE)

  if (setequal(before, after))
    return(FALSE)

  writeLines(after, con = path)
  messagef("* Wrote Python packages to '%s'.", path)
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
  messagef("* Restored Python packages from '%s'.", aliased_path(path))
}

renv_python_virtualenv_root <- function() {
  Sys.getenv("WORKON_HOME", unset = path.expand("~/.virtualenvs"))
}

renv_python_virtualenv_create <- function(path) {
  python <- renv_python_active_binary()
  version <- renv_python_version(python)
  module <- if (numeric_version(version) > "3.2") "venv" else "virtualenv"
  cmd <- paste(shQuote(python), "-m", module, shQuote(path))
  system(cmd)
  file.exists(path)
}

renv_python_is_virtualenv <- function(python) {
  paths <- c("activate_this.py", "pyvenv.cfg", "../pyvenv.cfg")
  any(file.exists(file.path(dirname(python), paths)))
}
