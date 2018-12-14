
renv_manifest_load <- function(project = NULL) {

  project <- renv_active_project(project)
  manifest <- renv_active_manifest(project)
  if (length(manifest))
    return(renv_manifest_read(manifest))

  renv_diagnose(project)

}

