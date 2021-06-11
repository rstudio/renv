
pip_freeze <- function(python) {
  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "freeze")
  renv_system_exec(python, args, "invoking pip freeze")
}

pip_install <- function(python, modules) {
  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "install", "--upgrade", modules)
  action <- paste("installing", paste(shQuote(modules), collapse = ", "))
  renv_system_exec(python, args, action)
}

pip_install_requirements <- function(python, requirements) {

  file <- renv_tempfile_path("renv-requirements-", fileext = ".txt")
  writeLines(requirements, con = file)
  on.exit(unlink(requirements), add = TRUE)

  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "install", "--upgrade", "-r", shQuote(file))
  action <- "restoring Python packages"
  renv_system_exec(python, args, action)

}

pip_uninstall <- function(python, modules) {
  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "uninstall", "--yes", modules)
  action <- paste("uninstalling", paste(shQuote(modules), collapse = ", "))
  renv_system_exec(python, args, action)
  TRUE
}
