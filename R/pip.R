
pip_freeze <- function(..., python = NULL) {
  python <- python %||% renv_python_active()
  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "freeze")
  action <- "invoking pip freeze"
  renv_system_exec(python, args, action, ...)
}

pip_install <- function(modules, ..., python = NULL) {
  python <- python %||% renv_python_active()
  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "install", "--upgrade", modules)
  action <- paste("installing", paste(shQuote(modules), collapse = ", "))
  renv_system_exec(python, args, action, ...)
}

pip_install_requirements <- function(requirements, ..., python = NULL) {

  python <- python %||% renv_python_active()

  file <- renv_scope_tempfile("renv-requirements-", fileext = ".txt")
  writeLines(requirements, con = file)

  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "install", "--upgrade", "-r", renv_shell_path(file))
  action <- "restoring Python packages"
  renv_system_exec(python, args, action, ...)

}

pip_uninstall <- function(modules, ..., python = NULL) {

  python <- python %||% renv_python_active()

  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "uninstall", "--yes", modules)
  action <- paste("uninstalling", paste(shQuote(modules), collapse = ", "))
  renv_system_exec(python, args, action, ...)

  TRUE

}
