
renv_python_resolve <- function(python = NULL) {

  # if Python was explicitly supplied, use it
  if (!is.null(python)) {

    resolved <- Sys.which(renv_path_canonicalize(python))
    if (nzchar(resolved))
      return(resolved)

    stopf("'%s' does not refer to a valid python interpreter", python)

  }

  # in interactive sessions, ask user what version of python they'd like to use
  if (interactive()) {

    python <- renv_python_select()

    fmt <- "* Selected %s [Python %s]."
    vwritef(fmt, renv_path_pretty(python), renv_python_version(python))

    return(path.expand(python))

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

renv_python_find <- function(version, path = NULL) {
  renv_python_find_impl(version, path)
}

renv_python_find_impl <- function(version, path = NULL) {

  # if we've been given the name of an environment,
  # check to see if it's already been initialized
  # and use the associated copy of Python if possible
  if (!is.null(path) && file.exists(path)) {
    python <- catch(renv_python_exe(path))
    if (!inherits(python, "error"))
      return(python)
  }

  # try to find a compatible version of python
  pythons <- renv_python_discover()
  if (length(pythons) == 0) {

    fmt <- lines(
      "project requested Python %s, but no compatible Python installation could be found.",
      "renv's Python integration will be disabled in this session.",
      "See `?renv::use_python` for more details."
    )

    msg <- sprintf(fmt, version)
    stop(msg)

  }

  # read python versions
  pyversions <- map_chr(pythons, function(python) {
    tryCatch(
      renv_python_version(python),
      error = function(e) "0.0.0"
    )
  })

  # try to find a compatible version
  renv_version_match(pyversions, version)

}

renv_python_exe <- function(path) {

  # if this already looks like a Python executable, use it directly
  info <- renv_file_info(path)
  if (identical(info$isdir, FALSE) && startswith(basename(path), "python"))
    return(renv_path_canonicalize(path))

  # otherwise, attempt to infer the Python executable type
  info <- renv_python_info(path)
  if (!is.null(info$python))
    return(renv_path_canonicalize(info$python))

  fmt <- "failed to find Python executable associated with path %s"
  stopf(fmt, renv_path_pretty(path))

}

renv_python_version <- function(python) {
  python <- normalizePath(python, winslash = "/", mustWork = TRUE)
  filebacked("python.versions", python, renv_python_version_impl)
}

renv_python_version_impl <- function(python) {
  python <- renv_path_canonicalize(python)
  code <- "from platform import python_version; print(python_version())"
  args <- c("-c", shQuote(code))
  action <- "reading Python version"
  renv_system_exec(python, args, action)
}

renv_python_info <- function(python) {

  found <- renv_file_find(python, function(path) {

    # check for virtual environment files
    virtualenv <-
      file.exists(file.path(path, "pyvenv.cfg")) ||
      file.exists(file.path(path, ".Python")) ||
      file.exists(file.path(path, "bin/activate_this.py"))

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

  if (!is.null(found))
    return(found)

  if (file.exists(python))
    list(python = python, type = "system", root = python)

}

renv_python_type <- function(python) {
  info <- renv_python_info(python)
  info$type
}

renv_python_action <- function(action, prompt, project) {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python) || !file.exists(python))
    return(NULL)

  type <- renv_python_type(python)
  if (is.null(type))
    return(NULL)

  if (type == "conda" && !requireNamespace("reticulate", quietly = TRUE))
    return(NULL)

  action(python, type, prompt, project)

}

renv_python_snapshot <- function(project, prompt) {
  renv_python_action(
    renv_python_snapshot_impl,
    prompt  = prompt,
    project = project
  )
}

renv_python_snapshot_impl <- function(python, type, prompt, project) {

  switch(type,
    virtualenv = renv_python_virtualenv_snapshot(project, prompt, python),
    conda      = renv_python_conda_snapshot(project, prompt, python)
  )

}

renv_python_restore <- function(project, prompt) {
  renv_python_action(
    renv_python_restore_impl,
    prompt  = prompt,
    project = project
  )
}

renv_python_restore_impl <- function(python, type, prompt, project) {

  case(
    type == "virtualenv" ~ renv_python_virtualenv_restore(project, prompt, python),
    type == "conda"      ~ renv_python_conda_restore(project, prompt, python)
  )

}

renv_python_envpath_virtualenv <- function(version) {
  majmin <- paste(renv_version_components(version, 1L:2L), collapse = ".")
  fmt <- "python/virtualenvs/renv-python-%s"
  sprintf(fmt, majmin)
}

renv_python_envpath_condaenv <- function(version) {
  "python/condaenvs/renv-python"
}

renv_python_envpath <- function(project, type, version = NULL) {

  suffix <- case(
    type == "virtualenv" ~ renv_python_envpath_virtualenv(version),
    type == "conda"      ~ renv_python_envpath_condaenv(version),
    ~ stopf("internal error: unrecognized environment type '%s'", type)
  )

  renv_paths_renv(suffix, project = project)

}

renv_python_envname <- function(project, path, type) {

  # check for a project-local environment
  if (renv_path_within(path, project)) {
    stem <- substring(path, nchar(project) + 2L)
    path <- paste(".", stem, sep = "/")
    return(path)
  }

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

renv_python_discover <- function() {

  all <- stack()

  # find python in some pre-determined root directories
  roots <- c(
    getOption("renv.python.root"),
    Sys.getenv("WORKON_HOME", "~/.virtualenvs"),
    "/opt/python",
    "/opt/local/python",
    "~/opt/python",
    file.path(renv_pyenv_root(), "versions")
  )

  for (root in roots) {
    versions <- sort(list.files(root, full.names = TRUE), decreasing = TRUE)
    exts <- if (renv_platform_windows()) "Scripts/python.exe" else "bin/python"
    pythons <- file.path(versions, exts)
    all$push(pythons)
  }

  # find Homebrew python
  if (renv_platform_macos()) {

    homebrew <- renv_homebrew_root()
    roots <- sort(list.files(
      path       = file.path(homebrew, "opt"),
      pattern    = "^python@[[:digit:]]+[.][[:digit:]]+$",
      full.names = TRUE
    ), decreasing = TRUE)

    for (root in roots) {

      # homebrew python doesn't install bin/python, so we need
      # to be a little bit more clever here
      exes <- list.files(
        path = file.path(root, "bin"),
        pattern = "^python[[:digit:]]+[.][[:digit:]]+$",
        full.names = TRUE
      )

      if (length(exes))
        all$push(exes[[1L]])

    }

  }

  # find Windows python installations
  if (renv_platform_windows()) {

    sd <- Sys.getenv("SYSTEMDRIVE", unset = "C:")
    roots <- file.path(sd, c("", "Program Files"))

    lad <- Sys.getenv("LOCALAPPDATA", unset = NA)
    if (!is.na(lad))
      roots <- c(roots, file.path(lad, "Programs/Python"))

    dirs <- list.files(
      path       = roots,
      pattern    = "^Python",
      full.names = TRUE
    )

    if (length(dirs)) {
      exes <- file.path(dirs, "python.exe")
      pythons <- normalizePath(exes, winslash = "/", mustWork = FALSE)
      all$push(pythons)
    }

  }

  # find Python installations on the PATH
  path <- Sys.getenv("PATH", unset = "")
  splat <- strsplit(path, .Platform$path.sep, fixed = TRUE)[[1L]]
  for (entry in splat) {
    for (exe in c("python3", "python")) {
      python <- Sys.which(file.path(entry, exe))
      if (nzchar(python))
        all$push(python)
    }
  }

  # collect discovered pythons as vector
  pythons <- unlist(all$data(), recursive = FALSE, use.names = TRUE)

  # don't include /usr/bin/python on macOS (too old)
  if (renv_platform_macos())
    pythons <- setdiff(pythons, "/usr/bin/python")

  # get list of pythons
  pythons <- renv_path_canonicalize(pythons[file.exists(pythons)])

  # don't include WindowsApps
  if (renv_platform_windows())
    pythons <- grep("/WindowsApps/", pythons, invert = TRUE, value = TRUE)

  unique(pythons)

}

renv_python_select_error <- function() {

  lines <- c(
    "renv was unable to find any Python installations on your machine.",
    if (renv_platform_windows())
      "Consider installing Python from https://www.python.org/downloads/windows/.",
    if (renv_platform_macos())
      "Consider installing Python from https://www.python.org/downloads/mac-osx/."
  )

  stop(paste(lines, collapse = "\n"))

}

renv_python_select <- function(candidates = NULL) {

  candidates <- aliased_path(candidates %||% renv_python_discover())
  if (empty(candidates))
    return(renv_python_select_error())

  title <- "Please select a version of Python to use with this project:"
  selection <- tryCatch(
    utils::select.list(candidates, title = title, graphics = FALSE),
    interrupt = identity
  )

  if (selection %in% "" || inherits(selection, "interrupt"))
    stop("operation canceled by user")

  return(path.expand(selection))

}

renv_python_module_available <- function(python, module) {
  python <- renv_path_canonicalize(python)
  command <- paste("import", module)
  args <- c("-c", shQuote(command))
  status <- system2(python, args, stdout = FALSE, stderr = FALSE)
  identical(status, 0L)
}

renv_python_active <- function() {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python))
    stop("internal error: RENV_PYTHON is not set")

  renv_python_validate(python)

}

renv_python_validate <- function(python) {

  if (!file.exists(python)) {
    fmt <- "%s does not exist"
    stopf(fmt, renv_path_pretty(python))
  }

  invisible(python)

}
