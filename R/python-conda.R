
renv_python_conda_select <- function(name) {

  # get python package
  version <- Sys.getenv("RENV_CONDA_PYTHON_VERSION", unset = "3.6")
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

renv_python_conda_snapshot <- function(project, python) {

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "environment.yml")

  # find the root of the associated conda environment
  lockfile <- renv_lockfile_load(project = project)
  name <- lockfile$Python$Name %||% renv_python_envpath(project, "conda", version)
  python <- renv_python_conda_select(name)
  info <- renv_python_info(python)
  prefix <- info$root

  conda <- reticulate::conda_binary()
  args <- c("env", "export", "--prefix", shQuote(prefix), "--file", shQuote(path))
  output <- if (renv_testing()) FALSE else ""
  system2(conda, args, stdout = output, stderr = output)

  vwritef("* Wrote Python packages to '%s'.", aliased_path(path))
  return(TRUE)
}

renv_python_conda_restore <- function(project, python) {

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  path <- file.path(project, "environment.yml")

  # find the root of the associated conda environment
  lockfile <- renv_lockfile_load(project = project)
  name <- lockfile$Python$Name %||% renv_python_envpath(project, "conda", version)
  python <- renv_python_conda_select(name)
  info <- renv_python_info(python)
  prefix <- info$root

  conda <- reticulate::conda_binary()
  cmd <- if (file.exists(prefix)) "update" else "create"
  args <- c("env", cmd, "--prefix", shQuote(prefix), "--file", shQuote(path))
  output <- if (renv_testing()) FALSE else ""
  system2(conda, args, stdout = output, stderr = output)

  return(TRUE)

}
