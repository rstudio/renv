
renv_manifest_load <- function(project = NULL) {

  project <- project %||% renv_active_project_get()
  manifest <- renv_active_manifest(project)
  if (file.exists(manifest))
    return(renv_manifest_read(manifest))

  renv_diagnose(project)

}

