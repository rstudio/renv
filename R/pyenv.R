
renv_pyenv_root <- function() {
  root <- Sys.getenv("PYENV_ROOT", unset = renv_pyenv_root_default())
  path.expand(root)
}

renv_pyenv_root_default <- function() {

  if (renv_platform_windows())
    "~/.pyenv/pyenv-win"
  else
    "~/.pyenv"

}

