
renv_python_resolve <- function(python = NULL) {

  # if Python was explicitly supplied, use it
  if (!is.null(python)) {

    python <- Sys.which(path.expand(python))
    if (nzchar(python))
      return(python)

    stopf("requested python '%s' is not available", python)

  }

  # check environment variables
  envvars <- c("RETICULATE_PYTHON", "RETICULATE_PYTHON_ENV")
  for (envvar in envvars) {
    val <- Sys.getenv(envvar, unset = NA)
    if (!is.na(val) && file.exists(val))
      return(val)
  }

  # check on the PATH (prefer Python 3)
  for (binary in c("python3", "python")) {
    python <- Sys.which(binary)
    if (nzchar(python))
      return(python)
  }

  stopf("could not locate Python (not available on the PATH)")

}

renv_python_compatible <- function(python, version) {

  actual <- renv_python_version(python = python)
  if (actual == version)
    return(TRUE)

  actual <- numeric_version(actual)
  expected <- numeric_version(version)

  if (actual[1, 1:2] == expected[1, 1:2])
    return(TRUE)

  FALSE

}

renv_python_find <- function(version, path = NULL) {

  # TODO: use renv_python_compatible() throughout

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
    python <- Sys.which(binary)
    if (nzchar(python))
      return(python)
  }

  # try to find a plain old python executable
  python <- Sys.which("python")
  if (nzchar(python) && renv_python_compatible(python, version))
    return(python)

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
  python <- renv_path_normalize(python)
  args <- c("-c", shQuote("from platform import python_version; print(python_version())"))
  system2(python, args, stdout = TRUE, stderr = TRUE)
}

renv_python_info <- function(python) {

  renv_file_find(python, function(path) {

    # check for virtual environment files
    virtualenv <-
      file.exists(file.path(path, "pyvenv.cfg")) ||
      file.exists(file.path(path, ".Python"))

    if (virtualenv) {
      suffix <- if (renv_platform_windows()) "Scripts/python.exe" else "bin/python"
      python <- file.path(path, suffix)
      return(list(python = python, type = "virtualenv", root = path))
    }

    # check for conda-meta
    condaenv <-
      file.exists(file.path(path, "conda-meta")) &&
      !file.exists(file.path(path, "condabin"))

    if (condaenv) {
      suffix <- if (renv_platform_windows()) "python.exe" else "bin/python"
      python <- file.path(path, suffix)
      return(list(python = python, type = "conda", root = path))
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

  switch(type,
    system     = renv_python_virtualenv_snapshot(project, python),
    virtualenv = renv_python_virtualenv_snapshot(project, python),
    conda      = renv_python_conda_snapshot(project, python)
  )

}

renv_python_restore <- function(project) {
  renv_python_action(renv_python_restore_impl, project = project)
}

renv_python_restore_impl <- function(python, type, project) {

  switch(type,
    virtualenv = renv_python_virtualenv_restore(project, python),
    conda      = renv_python_conda_restore(project, python)
  )

}

renv_python_envpath <- function(project, type, version) {

  suffix <- switch(type,
    virtualenv = sprintf("renv/python/virtualenvs/renv-python-%s", version),
    conda      = "renv/python/condaenvs/renv-python",
    stopf("unrecognized environment type '%s'", type)
  )

  file.path(project, suffix)

}

renv_python_envname <- function(project, path, type) {

  # we return NULL for environments within the project
  # as these names get auto-constructed from other metadata
  # related to the Python executable used
  if (renv_path_within(path, project))
    return(NULL)

  bn <- basename(path)

  # check for file within virtualenv
  ok <-
    type == "virtualenv" &&
    identical(renv_python_virtualenv_path(bn), path)

  if (ok)
    return(bn)

  # check for named conda environment
  ok <-
    type == "conda" &&
    bn %in% reticulate::conda_list()$name

  if (ok)
    return(bn)

  # doesn't match any known named environments; return full path
  path

}
