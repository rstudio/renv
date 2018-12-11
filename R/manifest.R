
renv_manifest_load <- function(project = NULL) {

  project <- renv_active_project(project)
  path <- file.path(project, "renv/manifest")
  if (file.exists(path))
    return(renv_manifest_read(path))

  renv_diagnose(project)

}

