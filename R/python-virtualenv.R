
renv_python_virtualenv_home <- function() {
  Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
}

renv_python_virtualenv_path <- function(name) {

  # if the name contains a slash, use it as-is
  if (grepl("/", name, fixed = TRUE))
    return(renv_path_canonicalize(name))

  # treat names starting with '.' specially
  if (substring(name, 1L, 1L) == ".")
    return(renv_path_canonicalize(name))

  # otherwise, resolve relative to virtualenv home
  home <- renv_python_virtualenv_home()
  file.path(home, name)

}

renv_python_virtualenv_validate <- function(path, version) {

  # get path to python executable
  python <- renv_python_exe(path)

  # compare requested + actual versions
  if (!is.null(version)) {
    request <- version
    current <- renv_python_version(python)
    if (!renv_version_eq(request, current, 2L)) {
      fmt <- "Project requested Python version '%s' but '%s' is currently being used"
      warningf(fmt, request, current)
    }
  }

  python

}

renv_python_virtualenv_create <- function(python, path) {

  ensure_parent_directory(path)

  python <- renv_path_normalize(python)
  version <- renv_python_version(python)
  module <- if (numeric_version(version) > "3.2") "venv" else "virtualenv"
  args <- c("-m", module, shQuote(path.expand(path)))
  output <- system2(python, args = args, stdout = TRUE, stderr = TRUE)

  status <- attr(output, "status") %||% 0L
  if (status != 0L || !file.exists(path)) {
    msg <- c("failed to create virtual environment", output)
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }

  info <- renv_python_info(path)
  info$python

}

renv_python_virtualenv_update <- function(python, packages = NULL) {

  # resolve python executable path
  python <- renv_python_exe(python)
  python <- renv_path_normalize(python)

  # resolve packages
  packages <- packages %||% c("pip", "setuptools", "wheel")

  # run upgrade command
  args <- c("-m", "pip", "install", "--upgrade", packages)
  output <- system2(python, args = args, stdout = TRUE, stderr = TRUE)

  status <- attr(output, "status") %||% 0L
  if (status != 0L) {
    msg <- c("failed to update python packages", output)
    warning(paste(msg, collapse = "\n"), call. = FALSE)
  }

}

renv_python_virtualenv_snapshot <- function(project, python) {

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "requirements.txt")
  before <- character()
  if (file.exists(path))
    before <- readLines(path, warn = FALSE)

  suffix <- "-m pip freeze 2> /dev/null"
  command <- paste(shQuote(python), suffix)
  after <- system(command, intern = TRUE)

  if (setequal(before, after)) {
    vwritef("* '%s' is already up to date.", aliased_path(path))
    return(FALSE)
  }

  writeLines(after, con = path)
  vwritef("* Wrote Python packages to '%s'.", aliased_path(path))
  return(TRUE)

}

renv_python_virtualenv_restore <- function(project, python) {

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "requirements.txt")
  before <- character()
  if (file.exists(path))
    before <- readLines(path, warn = FALSE)

  suffix <- "-m pip freeze 2> /dev/null"
  command <- paste(shQuote(python), suffix)
  after <- system(command, intern = TRUE)

  if (setequal(before, after)) {
    vwritef("* The Python library is already up to date.")
    return(FALSE)
  }

  diff <- renv_vector_diff(before, after)
  file <- renv_tempfile_path("renv-requirements-", fileext = ".txt")
  writeLines(diff, con = file)
  suffix <- paste("-m pip install --upgrade -r", shQuote(file))
  command <- paste(shQuote(python), suffix)
  ignore <- renv_tests_running()
  system(command, ignore.stdout = ignore, ignore.stderr = ignore)

  vwritef("* Restored Python packages from '%s'.", aliased_path(path))

}
