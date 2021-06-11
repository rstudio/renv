
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

  python <- renv_path_canonicalize(python)
  version <- renv_python_version(python)
  module <- if (numeric_version(version) > "3.2") "venv" else "virtualenv"
  args <- c("-m", module, shQuote(path.expand(path)))
  renv_system_exec(python, args, "creating virtual environment")

  info <- renv_python_info(path)
  info$python

}

renv_python_virtualenv_update <- function(python, packages = NULL) {

  # resolve python executable path
  python <- renv_python_exe(python)
  python <- renv_path_canonicalize(python)

  # resolve packages
  packages <- packages %||% c("pip", "setuptools", "wheel")
  status <- catch(pip_install(python, packages))
  if (inherits(status, "error"))
    warning(status)

  TRUE

}

renv_python_virtualenv_snapshot <- function(project, python) {

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "requirements.txt")
  before <- character()
  if (file.exists(path))
    before <- readLines(path, warn = FALSE)

  after <- pip_freeze(python)
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

  after <- pip_freeze(python)
  if (setequal(before, after)) {
    vwritef("* The Python library is already up to date.")
    return(FALSE)
  }

  diff <- renv_vector_diff(before, after)
  pip_install_requirements(python, diff)

  vwritef("* Restored Python packages from '%s'.", aliased_path(path))

}
