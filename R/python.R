
renv_python_find <- function(version, path = NULL) {

  # if we've been given the name of an environment,
  # check to see if it's already been initialized
  # and use the associated copy of Python if possible
  if (!is.null(path) && file.exists(path)) {
    python <- catch(renv_python_exe(path))
    if (!inherits(python, "error"))
      return(python)
  }

  # try to find a copy of python on the PATH
  idx <- gregexpr("(?:[.]|$)", version)[[1]]
  strings <- substring(version, 1, idx - 1)
  for (suffix in rev(strings)) {
    binary <- paste("python", suffix, sep = "")
    path <- Sys.which(binary)
    if (nzchar(path))
      return(path)
  }

  fmt <- "failed to find Python %s on the PATH"
  warningf(fmt, version)

  NULL

}

renv_python_exe <- function(path) {

  # if this already looks like a Python executable, use it directly
  info <- file.info(path, extra_cols = FALSE)
  if (identical(info$isdir, FALSE) && startswith(basename(path), "python"))
    return(path)

  # otherwise, attempt to infer the Python executable type
  info <- renv_python_info(path)
  if (!is.null(info$python))
    return(info$python)

  fmt <- "failed to find Python executable associated with path '%s'"
  stopf(fmt, aliased_path(path))

}

renv_python_version <- function(python) {
  python <- normalizePath(python)
  cmd <- paste(shQuote(python), "--version 2>&1")
  output <- catch(system(cmd, intern = TRUE))
  space <- regexpr(" ", output, fixed = TRUE)
  substring(output, space + 1)
}

renv_python_info <- function(python) {

  renv_file_find(python, function(path) {

    # check for virtual environment files
    virtualenv <-
      file.exists(file.path(path, "pyvenv.cfg")) ||
      file.exists(file.path(path, ".Python"))

    if (virtualenv) {
      suffix <- if (renv_platform_windows()) "Scripts/python.exe" else "bin/python"
      return(list(python = file.path(path, suffix), type = "virtualenv", root = path))
    }

    # check for conda-meta
    condaenv <-
      file.exists(file.path(path, "conda-meta")) &&
      !file.exists(file.path(path, "condabin"))

    if (condaenv) {
      suffix <- if (renv_platform_windows()) "python.exe" else "bin/python"
      return(list(python = file.path(path, suffix), type = "conda", root = path))
    }

  })

}

renv_python_type <- function(python) {
  renv_python_info(python)$type
}

renv_python_action <- function(action, project) {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python) || !file.exists(python))
    return(NULL)

  type <- renv_python_type(python)
  if (is.null(type))
    return(NULL)

  if (type == "conda" && !requireNamespace("reticulate", quietly = TRUE))
    return(NULL)

  action(python, type, project)

}

renv_python_snapshot <- function(project) {
  renv_python_action(renv_python_snapshot_impl, project = project)
}

renv_python_snapshot_impl <- function(python, type, project) {

  switch(
    type,
    virtualenv = renv_python_pip_freeze(project, python),
    conda      = renv_python_conda_list(project, python)
  )

}

renv_python_restore <- function(project) {
  renv_python_action(renv_python_restore_impl, project = project)
}

renv_python_restore_impl <- function(python, type, project) {

  switch(
    type,
    virtualenv = renv_python_pip_restore(project, python),
    conda      = renv_python_conda_restore(project, python)
  )

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
  file <- renv_tempfile("renv-requirements-", fileext = ".txt")
  writeLines(diff, con = file)
  suffix <- paste("-m pip install --upgrade -r", shQuote(file))
  command <- paste(shQuote(python), suffix)
  system(command)

  vwritef("* Restored Python packages from '%s'.", aliased_path(path))
}

renv_python_conda_list <- function(project, python) {
  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "requirements.txt")

  # find the root of the associated conda environment
  lockfile <- renv_lockfile_load(project = project)
  name <- lockfile$Python$Name %||% file.path(project, "renv/python/condaenvs/renv-condaenv")
  python <- renv_python_conda_select(name)
  info <- renv_python_info(python)
  prefix <- info$root

  conda <- reticulate::conda_binary()
  args <- c("list", "--prefix", shQuote(prefix), "--export")
  system2(conda, args, stdout = path)

  vwritef("* Wrote Python packages to '%s'.", aliased_path(path))
  return(TRUE)
}

renv_python_conda_restore <- function(project, python) {
  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "requirements.txt")

  # find the root of the associated conda environment
  lockfile <- renv_lockfile_load(project = project)
  name <- lockfile$Python$Name %||% file.path(project, "renv/python/condaenvs/renv-condaenv")
  python <- renv_python_conda_select(name)
  info <- renv_python_info(python)
  prefix <- info$root

  conda <- reticulate::conda_binary()
  args <- c("install", "--yes", "--prefix", shQuote(prefix), "--file", shQuote(path))
  system2(conda, args)

  return(TRUE)
}

renv_python_virtualenv_create <- function(python, path) {
  ensure_parent_directory(path)
  version <- renv_python_version(python)
  module <- if (numeric_version(version) > "3.2") "venv" else "virtualenv"
  fmt <- "%s -m %s %s 2>&1"
  python <- normalizePath(python)
  cmd <- sprintf(fmt, shQuote(python), module, shQuote(path.expand(path)))
  output <- system(cmd, intern = TRUE)
  status <- attr(output, "status") %||% 0L
  if (status != 0L || !file.exists(path)) {
    msg <- c("failed to create virtual environment", output)
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }
  invisible(file.exists(path))
}

renv_python_virtualenv_path <- function(name) {

  path <- name
  if (!grepl("[/\\\\]", name)) {
    home <- Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
    path <- file.path(home, name)
  }

  path

}

renv_python_conda_select <- function(name) {

  # handle paths (as opposed to environment names)
  if (grepl("[/\\\\]", name)) {
    if (!file.exists(name))
      return(reticulate::conda_create(envname = name))
    return(renv_python_exe(name))
  }

  # check for an existing conda environment
  envs <- reticulate::conda_list()
  idx <- which(name == envs$name)
  if (length(idx))
    return(envs$python[[idx]])

  # no environment exists; create it
  reticulate::conda_create(envname = name)

}
