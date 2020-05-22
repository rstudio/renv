
renv_addins_embed <- function() {
  project <- rstudioapi::getActiveProject()
  context <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = context$id)
  embed(path = context$path, project = context$project)
}

