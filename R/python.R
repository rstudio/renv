
renv_python_version <- function(python) {
  cmd <- paste(shQuote(python), "--version 2>&1")
  output <- catch(system(cmd, intern = TRUE))
  space <- regexpr(" ", output, fixed = TRUE)
  substring(output, space + 1)
}

renv_python_snapshot <- function(project) {

  python <- settings$python()
  if (is.null(python))
    return(NULL)

  python <- renv_python_resolve(python)
  renv_python_pip_freeze(project, python)

}

renv_python_restore <- function(project) {

  python <- settings$python()
  if (is.null(python))
    return(NULL)

  python <- renv_python_resolve(python)
  renv_python_pip_restore(project, python)

}

renv_python_virtualenv_root <- function() {
  Sys.getenv("WORKON_HOME", unset = path.expand("~/.virtualenvs"))
}

renv_python_resolve <- function(python) {

  # when set to TRUE, we should try to auto-resolve a version of Python to use
  if (identical(python, TRUE)) {

    if (!requireNamespace("reticulate", quietly = TRUE))
      install("reticulate")

    config <- catch(reticulate::py_config())
    if (inherits(config, "error"))
      return(NULL)

    return(config$python)

  }

  # if the user has requested a virtual environment by name, provide it
  if (!grepl("[/\\]", python)) {
    virtualenv <- file.path(renv_python_virtualenv_root(), python)
    if (renv_file_exists(virtualenv))
      python <- virtualenv
  }

  # if the requested path doesn't exist, just return it (upstream
  # will warn appropriately)
  info <- file.info(python, extra_cols = FALSE)
  if (is.na(info$isdir))
    return(python)

  # if python points to a plain file, then assume we received the
  # path to a Python binary
  if (identical(info$isdir, FALSE))
    return(python)

  # otherwise, we received a directory; assume this is the root
  # for a virtualenv and provide the associated Python
  if (Sys.info()[["sysname"]] == "Windows")
    file.path(python, "Scripts/python.exe")
  else
    file.path(python, "bin/python")

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
