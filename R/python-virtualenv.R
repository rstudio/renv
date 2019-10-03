
renv_python_virtualenv_path <- function(name) {

  path <- name
  if (!grepl("[/\\\\]", name)) {
    home <- Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
    path <- file.path(home, name)
  }

  path

}

renv_python_virtualenv_validate <- function(path, python, version) {

  # compare requested vs. actual versions of Python
  exe <- renv_python_exe(path)
  request <- version %||% renv_python_version(python)
  current <- renv_python_version(exe)
  if (request == current)
    return(exe)

  # err in automatic sessions
  if (!interactive()) {
    fmt <- "incompatible virtual environment already exists at path '%s'"
    stopf(fmt, path)
  }

  renv_pretty_print(
    sprintf("Requested version %s != %s", request, current),
    "This virtual environment has already been initialized with a different copy of Python.",
    "The old virtual environment will be overwritten."
  )

  if (!proceed()) {
    renv_scope_options(show.error.messages = FALSE)
    message("* Operation aborted.")
    stop("operation aborted", call. = FALSE)
  }

  unlink(path, recursive = TRUE)
  return("")

}

renv_python_virtualenv_create <- function(python, path) {

  ensure_parent_directory(path)

  version <- renv_python_version(python)
  module <- if (numeric_version(version) > "3.2") "venv" else "virtualenv"

  fmt <- "%s -m %s %s 2>&1"
  python <- renv_path_normalize(python)
  cmd <- sprintf(fmt, shQuote(python), module, shQuote(path.expand(path)))
  output <- system(cmd, intern = TRUE)
  status <- attr(output, "status") %||% 0L

  if (status != 0L || !file.exists(path)) {
    msg <- c("failed to create virtual environment", output)
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }

  invisible(file.exists(path))

}

renv_python_virtualenv_snapshot <- function(project, python) {

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "requirements.txt")
  before <- character()
  if (file.exists(path))
    before <- readLines(path, warn = FALSE)

  suffix <- "-m pip freeze 2> /dev/null"
  command <- paste(shQuote(python), suffix)
  after <- system(command, intern = TRUE)

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

  suffix <- "-m pip freeze 2> /dev/null"
  command <- paste(shQuote(python), suffix)
  after <- system(command, intern = TRUE)

  if (setequal(before, after)) {
    vwritef("* The Python library is already up to date.")
    return(FALSE)
  }

  diff <- renv_vector_diff(before, after)
  file <- renv_tempfile("renv-requirements-", fileext = ".txt")
  writeLines(diff, con = file)
  suffix <- paste("-m pip install --upgrade -r", shQuote(file))
  command <- paste(shQuote(python), suffix)
  ignore <- renv_testing()
  system(command, ignore.stdout = ignore, ignore.stderr = ignore)

  vwritef("* Restored Python packages from '%s'.", aliased_path(path))

}
