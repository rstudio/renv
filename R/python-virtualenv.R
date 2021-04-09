
renv_python_virtualenv_path <- function(name) {

  if (grepl("/", name, fixed = TRUE))
    return(name)

  home <- Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
  file.path(home, name)

}

renv_python_virtualenv_validate <- function(path, version) {

  # get path to python executable
  python <- renv_python_exe(path)

  # compare requested + actual versions
  request <- version
  current <- renv_python_version(python)
  if (request == current)
    return(python)

  # notify user that there's a mismatch in what the lockfile asked for,
  # and what renv was able to find to satisfy the request
  values = c(
    paste("Lockfile version: ", version),
    paste("Actual version:   ", version)
  )

  preamble <- "The virtual environment exists, but has an unexpected version."
  postamble <- c(
    "You may need to re-generate the virtual environment, or update the lockfile.",
    paste("Python path:", renv_path_pretty(python))
  )

  renv_pretty_print(values, preamble, postamble, wrap = FALSE)
  python

}

renv_python_virtualenv_create <- function(python, path) {

  ensure_parent_directory(path)

  version <- renv_python_version(python)
  module <- if (numeric_version(version) > "3.2") "venv" else "virtualenv"
  python <- renv_path_normalize(python)
  args <- c("-m", module, shQuote(path.expand(path)))
  output <- system2(python, args = args, stdout = TRUE, stderr = TRUE)

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
  file <- renv_tempfile_path("renv-requirements-", fileext = ".txt")
  writeLines(diff, con = file)
  suffix <- paste("-m pip install --upgrade -r", shQuote(file))
  command <- paste(shQuote(python), suffix)
  ignore <- renv_tests_running()
  system(command, ignore.stdout = ignore, ignore.stderr = ignore)

  vwritef("* Restored Python packages from '%s'.", aliased_path(path))

}
