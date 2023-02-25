
renv_python_conda_select <- function(name, version = NULL) {

  # get python package
  version <- version %||% Sys.getenv("RENV_CONDA_PYTHON_VERSION", unset = "3.7")
  packages <- paste("python", version, sep = "=")

  # handle paths (as opposed to environment names)
  if (grepl("[/\\\\]", name)) {
    if (!file.exists(name))
      return(reticulate::conda_create(envname = name, packages = packages))
    return(renv_python_exe(name))
  }

  # check for an existing conda environment
  envs <- reticulate::conda_list()
  idx <- which(name == envs$name)
  if (length(idx))
    return(envs$python[[idx]])

  # no environment exists; create it
  reticulate::conda_create(envname = name, packages = packages)

}

renv_python_conda_export_path <- function(project) {

  # check override
  override <- renv_paths_override("CONDA_EXPORT")
  if (!is.null(override))
    return(override)

  # use default
  file.path(project, "environment.yml")

}

# TODO: support prompt
renv_python_conda_snapshot <- function(project, prompt, python) {

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- renv_python_conda_export_path(project = project)

  # find the root of the associated conda environment
  lockfile <- renv_lockfile_load(project = project)
  name <- lockfile$Python$Name %||% renv_python_envpath(project, "conda", version)
  python <- renv_python_conda_select(name)
  info <- renv_python_info(python)
  prefix <- info$root

  conda <- reticulate::conda_binary()
  args <- c(
    "env", "export",
    "--prefix", renv_shell_path(prefix),
    "--file", renv_shell_path(path)
  )

  output <- if (renv_tests_running()) FALSE else ""
  system2(conda, args, stdout = output, stderr = output)

  vwritef("* Wrote Python packages to '%s'.", renv_path_aliased(path))
  return(TRUE)
}

# TODO: support prompt
renv_python_conda_restore <- function(project, prompt, python) {

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- renv_python_conda_export_path(project = project)

  # find the root of the associated conda environment
  lockfile <- renv_lockfile_load(project = project)
  name <- lockfile$Python$Name %||% renv_python_envpath(project, "conda", version)
  python <- renv_python_conda_select(name)
  info <- renv_python_info(python)
  prefix <- info$root

  conda <- reticulate::conda_binary()
  cmd <- if (file.exists(prefix)) "update" else "create"
  args <- c(
    "env", cmd,
    "--prefix", renv_shell_path(prefix),
    "--file", renv_shell_path(path)
  )

  output <- if (renv_tests_running()) FALSE else ""
  system2(conda, args, stdout = output, stderr = output)

  return(TRUE)

}
