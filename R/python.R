
renv_python_blueprint_resolve <- function(python) {

  python <- case(
    is.null(python)          ~ NULL,
    identical(python, FALSE) ~ NULL,
    identical(python, TRUE)  ~ Sys.which("python"),
    as.character(python)
  )

  python %&&% list(Path = python)

}

renv_python_virtualenv_root <- function() {
  Sys.getenv("WORKON_HOME", unset = path.expand("~/.virtualenvs"))
}

renv_python_resolve <- function(python) {

  # if the user has requested a virtual environment by name, provide it
  if (!grepl("[/\\]", python)) {
    virtualenv <- file.path(renv_python_virtualenv_root(), python)
    if (file.exists(virtualenv))
      python <- virtualenv
  }

  # if the requested path doesn't exist, just return it (upstream
  # will warn appropriately)
  info <- file.info(python, extra_cols = FALSE)
  if (is.na(info$isdir))
    return(python)

  # if python points to a plain file, then assume we received the
  # path to a Python binary
  if (identical(info$isdir, FALSE))
    return(python)

  # otherwise, we received a directory; assume this is the root
  # for a virtualenv and provide the associated Python
  if (Sys.info()[["sysname"]] == "Windows")
    file.path(python, "Scripts/python.exe")
  else
    file.path(python, "bin/python")

}

renv_python_pip_freeze <- function(python = NULL) {
  python <- python %||% renv_state$python()
  args <- c("-m", "pip", "freeze")
  output <- system2(python, args, stdout = TRUE)
  renv_read_properties(text = output, delimiter = "==")
}
