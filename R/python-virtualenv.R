
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
    request <- renv_version_maj_min(version)
    current <- renv_version_maj_min(renv_python_version(python))
    if (request != current) {
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
  args <- c("-m", module, renv_shell_path(path))
  renv_system_exec(python, args, "creating virtual environment")

  info <- renv_python_info(path)
  info$python

}

renv_python_virtualenv_update <- function(python) {

  # resolve python executable path
  python <- renv_python_exe(python)
  python <- renv_path_canonicalize(python)

  # resolve packages
  packages <- c("pip", "setuptools", "wheel")

  # don't upgrade these packages for older versions of python, as we may
  # end up installing versions of packages that aren't actually compatible
  # with the version of python we're running
  version <- renv_python_version(python)
  if (renv_version_lt(version, "3.6"))
    return(TRUE)

  # perform the install
  # make errors non-fatal as the environment will still be functional even
  # if we're not able to install or update these packages
  status <- catch(pip_install(packages, python = python))
  if (inherits(status, "error"))
    warnify(status)

  TRUE

}

renv_python_virtualenv_snapshot <- function(project, prompt, python) {

  renv_scope_wd(project)

  path <- file.path(project, "requirements.txt")
  before <- character()
  if (file.exists(path))
    before <- readLines(path, warn = FALSE)

  after <- pip_freeze(python = python)
  if (setequal(before, after)) {
    writef("- Python requirements are already up to date.")
    return(FALSE)
  }

  caution_bullets("The following will be written to requirements.txt:", after)

  cancel_if(prompt && !proceed())

  writeLines(after, con = path)

  fmt <- "- Wrote Python packages to %s."
  writef(fmt, renv_path_pretty(path))
  return(TRUE)

}

renv_python_virtualenv_restore <- function(project, prompt, python) {

  renv_scope_wd(project)

  path <- file.path(project, "requirements.txt")
  if (!file.exists(path))
    return(FALSE)

  saved <- readLines(path, warn = FALSE)
  current <- pip_freeze(python = python)
  diff <- renv_vector_diff(saved, current)
  if (empty(diff)) {
    writef("- The Python library is already up to date.")
    return(FALSE)
  }

  caution_bullets("The following Python packages will be restored:", diff)
  cancel_if(prompt && !proceed())

  pip_install_requirements(saved, python = python, stream = TRUE)
  TRUE

}
