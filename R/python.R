
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
    virtualenv = renv_python_virtualenv_snapshot(project, python),
    conda      = renv_python_conda_snapshot(project, python)
  )

}

renv_python_restore <- function(project) {
  renv_python_action(renv_python_restore_impl, project = project)
}

renv_python_restore_impl <- function(python, type, project) {

  switch(
    type,
    virtualenv = renv_python_virtualenv_restore(project, python),
    conda      = renv_python_conda_restore(project, python)
  )

}
