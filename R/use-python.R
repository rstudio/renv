
#' Use Python
#'
#' Associate a version of Python with your project. When active, `renv` will
#' take care of capturing your Python dependencies when `renv::snapshot()` is called,
#' and re-installing your Python dependencies when `renv::restore()` is called.
#'
#' @inheritParams renv-params
#'
#' @param python The path to a Python binary. This can be the path to a Python
#'   binary on the system, or the path to a Python binary within an
#'   already-existing Python environment. If `NULL`, the `RETICULATE_PYTHON`
#'   environment variable is checked; if that is not set, then the default
#'   version of `python` on the `PATH` is used instead.
#'
#' @param type The type of Python environment to use. By default, a project-local
#'   virtual environment will be created with the requested version of Python,
#'   as supplied by `python`. Ignored if the requested version of `python` lives
#'   within a pre-existing Python environment.
#'
#' @param name When `type` is `"virtualenv"` or `"conda"`, this argument can be
#'   used to supply the name or path that should be used for the associated
#'   Python environment. Ignored if the requested version of `python` lives
#'   within a pre-existing Python environment.
#'
#' @param ... Optional arguments; currently unused.
#'
#' @export
use_python <- function(python = NULL,
                       type = "virtualenv",
                       name = NULL,
                       ...,
                       project = NULL)
{
  project <- project %||% renv_project()

  # resolve path to Python
  python <- python %||%
    Sys.getenv("RETICULATE_PYTHON", unset = NA) %NA%
    Sys.getenv("RETICULATE_PYTHON_ENV", unset = NA) %NA%
    Sys.which("python")

  # validate we have a real path to Python
  if (!file.exists(python)) local({

    if (nzchar(python %||% ""))
      stopf("requested Python '%s' does not exist or cannot be found", python)

    stopf("failed to resolve path to Python executable")

  })

  # construct path to Python executable
  python <- renv_python_exe(python) %||% python

  # form the lockfile entry to be written
  type <- renv_python_type(python) %||% type
  version <- renv_python_version(python)

  # form the lockfile fields we'll want to write
  fields <- list()
  fields$Version <- version
  fields$Type    <- type
  fields$Name    <- name

  # update the lockfile
  lockpath <- file.path(project, "renv.lock")
  lockfile <- if (file.exists(lockpath))
    renv_lockfile_read(lockpath)
  else
    renv_lockfile_init()

  if (!identical(fields, lockfile$Python)) {
    lockfile$Python <- fields
    renv_lockfile_write(lockfile, file = lockpath)
  }

  # if a Python virtual environment or conda environment was requested,
  # check for existence; if it doesn't exist create it now and update
  # the Python binary path (since we need to save that locally)
  python <- case(
    is.null(type)        ~ python,
    type == "virtualenv" ~ renv_use_python_virtualenv(project, python, version, name),
    type == "conda"      ~ renv_use_python_conda(project, python, version, name)
  )

  # re-initialize with these settings
  renv_load_python(fields)

  # notify user
  if (identical(type, "virtualenv")) {
    if (is.null(name))
      vwritef("* Python initialized. Using local virtual environment.")
    else
      vwritef("* Python initialized. Using virtual environment '%s'.", name)
  } else if (identical(type, "conda")) {
    if (is.null(name))
      vwritef("* Python initialized. Using local conda environment.")
    else
      vwritef("* Python initialized. Using conda environment '%s'.", name)
  } else {
    vwritef("* Python initialized. Using Python '%s' [%s].", python, version)
  }

  # report to user
  setwd(project)
  activate(project = project)

  invisible(TRUE)

}

renv_use_python_virtualenv <- function(project,
                                       python = NULL,
                                       version = NULL,
                                       name = NULL)
{
  # when name is NULL, use a local path
  # TODO: think about allowing multiple environments?
  name <- name %||% file.path(project, "renv/python/virtualenvs/renv-virtualenv")
  path <- renv_python_virtualenv_path(name)

  # create virtual environment if none exists
  if (!file.exists(path)) {
    vprintf("* Creating virtual environment '%s' ... ", basename(name))
    python <- python %||% renv_python_find(version, path)
    renv_python_virtualenv_create(python, path)
    vwritef("Done!")
  }

  # TODO: what if the environment already exists, but the Python version
  # is incompatible with the requested version? do we prompt to re-initialize
  # the environment? or something else?
  exe <- renv_python_exe(path)
  request <- version %||% renv_python_version(python)
  current <- renv_python_version(exe)
  if (request != current) {
    fmt <- "Requested Python version '%s' does not match current Python version '%s'."
    warningf(fmt, request, current)
  }

  # use the version of Python in the virtual environment
  exe
}

renv_use_python_conda <- function(project,
                                  python = NULL,
                                  version = NULL,
                                  name = NULL)
{
  if (!requireNamespace("reticulate", quietly = TRUE))
    stopf("use of conda environments requires the 'reticulate' package")

  # TODO: how to handle things like a requested Python version here?
  name <- name %||% file.path(project, "renv/python/condaenvs/renv-condaenv")
  renv_python_conda_select(name)

}
