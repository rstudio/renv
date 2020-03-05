
#' Use Python
#'
#' Associate a version of Python with your project.
#'
#' When Python integration is active, `renv` will:
#'
#' - Save metadata about the requested version of Python in `renv.lock` -- in
#'   particular, the Python version, and the Python type ("virtualenv", "conda",
#'   "system"),
#'
#' - On load, set the `RETICULATE_PYTHON` environment variable, so that the
#'   `reticulate` package can automatically use the requested copy of Python
#'   as appropriate,
#'
#' - Capture the set of installed Python packages during `renv::snapshot()`,
#'
#' - Reinstall the set of recorded Python packages during `renv::restore()`.
#'
#' @inherit renv-params
#'
#' @param ... Optional arguments; currently unused.
#'
#' @param python The path to a Python binary. This can be the path to a Python
#'   binary on the system, or the path to a Python binary within an
#'   already-existing Python environment. If `NULL`, the `RETICULATE_PYTHON`
#'   environment variable is checked; if that is not set, then the default
#'   version of `python` on the `PATH` is used instead. As a special case,
#'   `use_python(FALSE)` can be used to deactivate Python integration with
#'   a project.
#'
#' @param type The type of Python environment to use. When `"auto"` (the
#'   default), a project-local environment (virtual environments on Linux /
#'   macOS; conda environments on Windows) will be created. Ignored if the
#'   requested version of `python` lives within a pre-existing Python
#'   environment.
#'
#' @param name The name or path that should be used for the associated Python
#'   environment. If `NULL` and `python` points to a Python executable living
#'   within a pre-existing virtual environment, that environment will be used.
#'   Otherwise, a project-local environment will be created instead.
#'
#' @return `TRUE`, indicating that the requested version of Python has been
#'   successfully activated. Note that this function is normally called for
#'   its side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # use python with a project
#' renv::use_python()
#'
#' # use virtualenv python with a project
#' renv::use_python(type = "virtualenv")
#'
#' # use conda python with a project
#' renv::use_python(type = "conda")
#'
#' }
use_python <- function(python = NULL,
                       ...,
                       type = c("auto", "virtualenv", "conda", "system"),
                       name = NULL,
                       project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)
  project <- renv_project_resolve(project)

  if (identical(python, FALSE))
    return(renv_python_deactivate(project))

  # resolve path to Python
  python <- renv_python_resolve(python)

  # validate we have a real path to Python
  if (!file.exists(python)) local({

    if (nzchar(python %||% ""))
      stopf("requested Python '%s' does not exist or cannot be found", python)

    stopf("failed to resolve path to Python executable")

  })

  # construct path to Python executable
  python <- renv_python_exe(python) %||% python
  version <- renv_python_version(python)

  # build information about the version of python requested
  info <- renv_python_info(python)
  type <- info$type %||% match.arg(type)

  # handle 'auto' type
  if (identical(type, "auto"))
    type <- ifelse(renv_platform_windows(), "conda", "virtualenv")

  # form the lockfile fields we'll want to write
  fields <- list()
  fields$Version <- version
  fields$Type    <- type
  fields$Name    <- name

  # if a Python virtual environment or conda environment was requested,
  # check for existence; if it doesn't exist create it now
  if (type != "system") {
    name <- name %||% renv_python_envpath(project, type, version)
    python <- case(
      type == "virtualenv" ~ renv_use_python_virtualenv(project, name, version, python),
      type == "conda"      ~ renv_use_python_condaenv(project, name, version, python)
    )
  }

  # update the lockfile
  lockfile <- renv_lockfile_load(project)
  if (!identical(fields, lockfile$Python)) {
    lockfile$Python <- fields
    renv_lockfile_save(lockfile, project)
  }

  # re-initialize with these settings
  renv_load_python(project, fields)

  # notify user
  if (!renv_testing()) {
    if (is.null(type)) {
      fmt <- "* Activated Python %s (%s)."
      vwritef(fmt, version, aliased_path(python))
    } else {
      fmt <- "* Activated Python %s [%s; %s]"
      vwritef(fmt, version, type, aliased_path(name))
    }
  }

  # report to user
  setwd(project)
  activate(project = project)

  invisible(TRUE)

}

renv_use_python_virtualenv <- function(project, name, version = NULL, python = NULL) {

  name <- name %||% renv_python_envpath(project, "virtualenv", version)
  path <- renv_python_virtualenv_path(name)
  python <- python %||% renv_python_find(version, path)

  # if the environment already exists, but is associated with a different
  # version of Python, prompt the user to re-create that environment
  if (file.exists(path)) {
    exe <- renv_python_virtualenv_validate(path, python, version)
    if (file.exists(exe))
      return(exe)
  }

  # if no virtual environment exists, create it
  vprintf("* Creating virtual environment '%s' ... ", basename(name))
  renv_python_virtualenv_create(python, path)
  vwritef("Done!")
  renv_python_exe(path)

}

renv_use_python_condaenv <- function(project, name, version = NULL, python = NULL) {

  if (!requireNamespace("reticulate", quietly = TRUE))
    stopf("use of conda environments requires the 'reticulate' package")

  # TODO: how to handle things like a requested Python version here?
  name <- name %||% renv_python_envpath(project, "conda", version)
  renv_python_conda_select(name)

}

renv_python_deactivate <- function(project) {

  file <- renv_lockfile_path(project)
  if (!file.exists(file))
    return(TRUE)

  lockfile <- renv_lockfile_read(file)
  if (is.null(lockfile$Python))
    return(TRUE)

  lockfile$Python <- NULL
  renv_lockfile_write(lockfile, file = file)
  vwritef("* Deactived Python -- the lockfile has been updated.")
  TRUE

}
