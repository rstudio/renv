
#' Use python
#'
#' Associate a version of Python with your project.
#'
#' When Python integration is active, renv will:
#'
#' - Save metadata about the requested version of Python in `renv.lock` -- in
#'   particular, the Python version, and the Python type ("virtualenv", "conda",
#'   "system"),
#'
#' - Capture the set of installed Python packages during `renv::snapshot()`,
#'
#' - Re-install the set of recorded Python packages during `renv::restore()`.
#'
#' In addition, when the project is loaded, the following actions will be taken:
#'
#' - The `RENV_PYTHON` environment variable will be set, indicating the version
#'   of Python currently active for this sessions,
#'
#' - The `RETICULATE_PYTHON` environment variable will be set, so that the
#'   reticulate package can automatically use the requested copy of Python
#'   as appropriate,
#'
#' - The requested version of Python will be placed on the `PATH`, so that
#'   attempts to invoke Python will resolve to the expected version of Python.
#'
#' You can override the version of Python used in a particular project by
#' setting the `RENV_PYTHON` environment variable; e.g. as part of the
#' project's `.Renviron` file. This can be useful if you find that renv
#' is unable to automatically discover a compatible version of Python to
#' be used in the project.
#'
#' @inherit renv-params
#'
#' @param ... Optional arguments; currently unused.
#'
#' @param python
#'   The path to the version of Python to be used with this project. See
#'   **Finding Python** for more details.
#'
#' @param type
#'   The type of Python environment to use. When `"auto"` (the default),
#'   virtual environments will be used.
#'
#' @param name
#'   The name or path that should be used for the associated Python environment.
#'   If `NULL` and `python` points to a Python executable living within a
#'   pre-existing virtual environment, that environment will be used. Otherwise,
#'   a project-local environment will be created instead, using a name
#'   generated from the associated version of Python.
#'
#' @details
#' # Finding Python
#'
#' In interactive sessions, when `python = NULL`, renv will prompt for an
#' appropriate version of Python. renv will search a pre-defined set of
#' locations when attempting to find Python installations on the system:
#'
#' - `getOption("renv.python.root")`,
#' - `/opt/python`,
#' - `/opt/local/python`,
#' - `~/opt/python`,
#' - `/usr/local/opt` (for macOS Homebrew-installed copies of Python),
#' - `/opt/homebrew/opt` (for M1 macOS Homebrew-installed copies of Python),
#' - `~/.pyenv/versions`,
#' - Python instances available on the `PATH`.
#'
#' In non-interactive sessions, renv will first check the `RETICULATE_PYTHON`
#' environment variable; if that is unset, renv will look for Python on the
#' `PATH`. It is recommended that the version of Python to be used is explicitly
#' supplied for non-interactive usages of `use_python()`.
#'
#'
#' # Warning
#'
#' We strongly recommend using Python virtual environments, for a few reasons:
#'
#' 1. If something goes wrong with a local virtual environment, you can safely
#'    delete that virtual environment, and then re-initialize it later, without
#'    worry that doing so might impact other software on your system.
#'
#' 2. If you choose to use a "system" installation of Python, then any packages
#'    you install or upgrade will be visible to any other application that
#'    wants to use that same Python installation. Using a virtual environment
#'    ensures that any changes made are isolated to that environment only.
#'
#' 3. Choosing to use Anaconda will likely invite extra frustration in the
#'    future, as you may be required to upgrade and manage your Anaconda
#'    installation as new versions of Anaconda are released. In addition,
#'    Anaconda installations tend to work poorly with software not specifically
#'    installed as part of that same Anaconda installation.
#'
#' In other words, we recommend selecting "system" or "conda" only if you are an
#' expert Python user who is already accustomed to managing Python / Anaconda
#' installations on your own.
#'
#'
#' @return
#'   `TRUE`, indicating that the requested version of Python has been
#'   successfully activated. Note that this function is normally called for its
#'   side effects.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # use python with a project
#' renv::use_python()
#'
#' # use python with a project; create the environment
#' # within the project directory in the '.venv' folder
#' renv::use_python(name = ".venv")
#'
#' # use python with a pre-existing virtual environment located elsewhere
#' renv::use_python(name = "~/.virtualenvs/env")
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
                       name    = NULL,
                       project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)
  project <- renv_project_resolve(project)

  # deactivate python integration when FALSE
  if (identical(python, FALSE))
    return(renv_python_deactivate(project))

  # handle 'auto' type
  type <- match.arg(type)
  if (identical(type, "auto"))
    type <- "virtualenv"

  case(
    type == "system"     ~ renv_use_python_system(python, name, project),
    type == "virtualenv" ~ renv_use_python_virtualenv(python, name, project),
    type == "conda"      ~ renv_use_python_condaenv(python, name, project)
  )
}

renv_use_python_system <- function(python,
                                   name,
                                   project)
{
  # retrieve python information
  python <- renv_python_resolve(python)
  version <- renv_python_version(python)
  info <- renv_python_info(python)

  # if the user ended up selecting a virtualenv or conda python, then
  # just activate those and ignore the 'system' request
  if (identical(info$type, "virtualenv"))
    return(renv_use_python_virtualenv(info$python, name, project))
  if (identical(info$type, "conda"))
    return(renv_use_python_condaenv(info$python, name, project))

  # for 'system' python usages, we just use the path to python
  # (note that this may not be portable or useful for other machines)
  renv_use_python_fini(info, python, version, project)
}

renv_use_python_virtualenv <- function(python,
                                       name,
                                       project)
{
  # if name has been set, check and see if it refers to an already-existing
  # virtual environment; if that exists, use it
  if (is.null(python) && !is.null(name)) {
    path <- renv_python_virtualenv_path(name)
    if (file.exists(path))
      python <- renv_python_exe(name)
  }

  python  <- renv_python_resolve(python)
  version <- renv_python_version(python)
  info    <- renv_python_info(python)

  # if name is unset, and 'python' doesn't already refer to an existing
  # virtual environment, then we'll use a local virtual environment
  local <- is.null(name) && identical(info$type, "virtualenv")
  if (local) {
    name <- renv_path_aliased(info$root)
    if (renv_path_same(dirname(name), renv_python_virtualenv_home()))
      name <- basename(name)
  } else {
    name <- name %||% renv_python_envpath(project, "virtualenv", version)
    if (grepl("/", name, fixed = TRUE))
      name <- renv_path_canonicalize(name)
  }

  # now, check to see if the python environment exists;
  # if it does not exist, we'll create it now
  vpython <- renv_use_python_virtualenv_impl(project, name, version, python)
  vinfo <- renv_python_info(vpython)

  # finish up now
  renv_use_python_fini(vinfo, name, version, project)

}

renv_use_python_condaenv <- function(python,
                                     name,
                                     project)
{
  # if python is set, see if it's already the path to a python interpreter
  # living within a conda environment
  while (!is.null(python)) {

    if (!is.null(name)) {
      fmt <- "ignoring value of name %s as python was already set"
      warningf(fmt, renv_path_pretty(name))
    }

    # validate that this is a conda python
    info <- renv_python_info(python)
    if (!identical(info$type, "conda")) {
      fmt <- "%s does not appear to refer to a Conda instance of Python; ignoring"
      warningf(fmt, renv_path_pretty(python))
      break
    }

    # use this edition of python without further adieu
    version <- renv_python_version(python)
    return(renv_use_python_fini(info, name, version, project))

  }

  # TODO: how do we select which version of python we want to use?
  name <- name %||% renv_python_envpath(project, "conda")
  python <- renv_use_python_condaenv_impl(project, name)
  info <- renv_python_info(python)
  version <- renv_python_version(python)

  renv_use_python_fini(info, name, version, project)

}

renv_use_python_fini <- function(info, name, version, project) {

  # normalize project path for later comparison
  project <- renv_path_normalize(project)

  # handle 'name' -- treat values containing slashes specially, and
  # check if those paths are project-relative environments
  if (!is.null(name) && grepl("/", name, fixed = TRUE)) {
    name <- renv_path_normalize(name)
    if (startsWith(name, project)) {
      base <- substring(name, nchar(project) + 2L)
      name <- if (grepl("^[.][^/]+$", base)) base else file.path(".", base)
    }
  }

  # form the lockfile fields we'll want to write
  fields <- as.list(c(Version = version, Type = info$type, Name = name))

  # update the lockfile
  lockfile <- renv_lockfile_load(project)
  if (!identical(fields, lockfile$Python)) {
    lockfile$Python <- fields
    renv_lockfile_save(lockfile, project)
  }

  # re-initialize with these settings
  renv_load_python(project, fields)

  # notify user
  if (!renv_tests_running()) {
    if (is.null(info$type)) {
      fmt <- "- Activated Python %s (%s)."
      writef(fmt, version, renv_path_aliased(info$python))
    } else {
      fmt <- "- Activated Python %s [%s; %s]"
      writef(fmt, version, info$type, renv_path_aliased(name))
    }
  }

  # report to user
  setwd(project)
  activate(project = project)

  invisible(info$python)

}

# return the path to an existing python binary associated with the virtual
# environment having name 'name' and version 'version', or "" if no such
# python instance exists
renv_use_python_virtualenv_impl_existing <- function(project,
                                                     name = NULL,
                                                     version = NULL)
{
  # resolve environment path from name
  name <- name %||% renv_python_envpath(project, "virtualenv", version)
  path <- renv_python_virtualenv_path(name)
  if (!file.exists(path))
    return("")

  # check that this appears to have a valid python executable
  info <- catch(renv_python_info(path))
  if (inherits(info, "error")) {
    warning(info)
    return("")
  }

  # validate version and return
  renv_python_virtualenv_validate(path, version)
}

# Internal helper for activating a Python virtual environment
#
# @param project
#   The project directory.
#
# @param name
#   The environment name, if any. If unset, it should be constructed
#   based on the Python executable used (note: _not_ the version parameter)
#
# @param version
#   The _requested_ version of Python (which may not be the actual version!)
#   This version should be used as a hint for finding an appropriate version
#   of Python, if the environment needs to be re-created.
#
# @param python
#   The copy of Python to be used. When unset, an appropriate version of Python
#   should be discovered based on the `version` parameter.
#
# @return
#   The path to the Python binary in the associated virtual environment.
#
renv_use_python_virtualenv_impl <- function(project,
                                            name = NULL,
                                            version = NULL,
                                            python = NULL)
{
  # first, look for an already-existing python installation
  # associated with the requested version of python
  exe <- renv_use_python_virtualenv_impl_existing(project, name, version)
  if (file.exists(exe))
    return(exe)

  # couldn't resolve environment from requested version; try to find
  # a compatible version of python and re-create that environment
  python <- python %||% renv_python_find(version)
  pyversion <- renv_python_version(python)
  name <- name %||% renv_python_envpath(project, "virtualenv", pyversion)
  path <- renv_python_virtualenv_path(name)

  # if the environment already exists, but is associated with a different
  # version of Python, prompt the user to re-create that environment
  if (file.exists(path)) {
    exe <- renv_python_virtualenv_validate(path, version)
    if (file.exists(exe))
      return(exe)
  }

  printf("- Creating virtual environment '%s' ... ", basename(name))
  vpython <- renv_python_virtualenv_create(python, path)
  writef("Done!")

  printf("- Updating Python packages ... ")
  renv_python_virtualenv_update(vpython)
  writef("Done!")

  renv_python_virtualenv_validate(path, version)

}

renv_use_python_condaenv_impl <- function(project,
                                          name = NULL,
                                          version = NULL,
                                          python = NULL)
{
  # if we can't load reticulate, try installing if there is a version
  # recorded in the lockfile
  if (!requireNamespace("reticulate", quietly = TRUE)) {

    # retrieve reticulate record
    lockfile <- renv_lockfile_load(project = project)
    records <- renv_lockfile_records(lockfile)
    reticulate <- records[["reticulate"]]

    # if we have a reticulate record, then attempt to restore
    if (!is.null(reticulate)) {
      restore(packages = "reticulate",
              prompt = FALSE,
              project = project)
    } else {
      install(packages = "reticulate",
              prompt = FALSE,
              project = project)
    }

  }

  # try once more to load reticulate
  if (!requireNamespace("reticulate", quietly = TRUE))
    stopf("use of conda environments requires the 'reticulate' package")

  # TODO: how to handle things like a requested Python version here?
  name <- name %||% renv_python_envpath(project, "conda", version)
  renv_python_conda_select(name, version)
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
  writef("- Deactived Python -- the lockfile has been updated.")
  TRUE

}
