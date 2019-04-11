
renv_python_find <- function(version) {

  # if a python binary has been explicitly registered, use it
  python <- settings$python()
  if (!is.null(python))
    return(python)

  # otherwise, try to find a copy of python on the PATH
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
  type <- renv_python_type(path)
  if (is.null(type)) {
    fmt <- "path '%s' does not appear to be associated with a Python environment"
    stopf(fmt, aliased_path(path))
  }

  # use the root path inferred in the lookup
  root <- attr(type, "path")

  if (type == "conda") {
    suffix <- if (renv_platform_windows()) "python.exe" else "bin/python"
    return(file.path(root, suffix))
  }

  if (type == "virtualenv") {
    suffix <- if (renv_platform_windows()) "Scripts/python.exe" else "bin/python"
    return(file.path(root, suffix))
  }

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

renv_python_type <- function(python) {

  renv_file_find(python, function(path) {

    # check for virtual environment files
    venv <- c("pyvenv.cfg", ".Python")
    if (any(file.exists(file.path(path, venv))))
      return("virtualenv")

    # check for conda-meta
    conda <- c("conda-meta")
    if (any(file.exists(file.path(path, conda))))
      return("conda")

  })

}

renv_python_action <- function(action, project) {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python) || !file.exists(python))
    return(NULL)

  type <- renv_python_type(python)
  if (is.null(type))
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
  file <- tempfile("renv-requirements-", fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  writeLines(diff, con = file)
  suffix <- paste("-m pip install --upgrade -r", shQuote(file))
  command <- paste(shQuote(python), suffix)
  system(command)

  path <- aliased_path(file.path(project, "requirements.txt"))
  vwritef("* Restored Python packages from '%s'.", aliased_path(path))
}

renv_python_conda_list <- function(project, python) {
  stop("not yet implemented")
}

renv_python_conda_restore <- function(project, python) {
  stop("not yet implemented")
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
