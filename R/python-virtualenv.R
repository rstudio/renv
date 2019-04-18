
renv_python_virtualenv_path <- function(name) {

  path <- name
  if (!grepl("[/\\\\]", name)) {
    home <- Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
    path <- file.path(home, name)
  }

  path

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

renv_python_virtualenv_snapshot <- function(project, python) {

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

renv_python_virtualenv_restore <- function(project, python) {

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
