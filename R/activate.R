
renv_activate_read <- function(project = NULL, field = NULL) {
  project <- project %||% renv_active_project_get()

  path <- file.path(project, "renv/activate.dcf")
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
