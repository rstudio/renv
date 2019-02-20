
# helpers for interacting with 'activate.dcf', which contains the base
# set of information needed when loading a project
renv_activate_path <- function(project = NULL) {
  project <- project %||% renv_state$project()
  file.path(project, "renv/activate.dcf")
}

renv_activate_read <- function(project = NULL, field = NULL) {
  path <- renv_activate_path(project)
  ensure_existing_file(path)

  dcf <- lapply(renv_dcf_read(path), function(item) {
    if (item %in% c("TRUE", "FALSE"))
      return(as.logical(item))
    item
  })

  if (is.null(field))
    return(dcf)

  dcf[[field]]
}
