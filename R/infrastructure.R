renv_write_infrastructure <- function(project = NULL, renv) {
  project <- renv_active_project(project)

  renv_write_rprofile(project)
  renv_write_rbuildignore(project)
  renv_write_activate(project, renv)
  renv_write_active(project, renv)
}


renv_write_rprofile <- function(project) {

  renv_write_entry_impl(
    sprintf("source(\"%s\")", renv_paths_local_activate()),
    file.path(project, ".Rprofile"),
    TRUE
  )
}

renv_write_rbuildignore <- function(project) {

  renv_write_entry_impl(
    renv_paths_local_rbuildignore(),
    file.path(project, ".Rbuildignore"),
    FALSE
  )

}

renv_write_activate <- function(project = NULL, renv) {
  project <- renv_active_project(project)

  source <- system.file("resources/activate.R", package = "renv")
  target <- renv_paths_local(project, "activate.R")

  template <- readLines(source, warn = FALSE)
  rendered <- sprintf(template, renv_paths_subdir())

  if (file.exists(target)) {
    current <- readLines(source, warn = FALSE)
    if (identical(current, rendered))
      return(TRUE)
  }

  ensure_parent_directory(target)
  writeLines(rendered, target)

}

renv_write_active <- function(project = NULL, renv) {
  project <- renv_active_project(project)

  active <- renv_paths_local(project, "active")
  ensure_parent_directory(active)

  if (!file.exists(active)) {
    writeLines(renv, con = active)
    return(TRUE)
  }

  contents <- readLines(active)
  if (identical(contents, renv))
    return(TRUE)

  writeLines(renv, con = active)
  TRUE

}


renv_write_entry_impl <- function(line, file, force) {

  # check to see if file doesn't exist
  if (!file.exists(file)) {

    # if we're not forcing file creation, just bail
    if (!force)
      return(TRUE)

    # otherwise, write the file
    writeLines(line, file)
    return(TRUE)
  }

  # if the file already has the requested line, nothing to do
  contents <- readLines(file, warn = FALSE)
  exists <- any(grepl(line, contents, fixed = TRUE))
  if (exists)
    return(TRUE)

  # add the loader and write to file
  contents <- c(contents, line)
  writeLines(contents, file)
  TRUE

}



renv_remove_infrastructure <- function(project = NULL) {
  project <- renv_active_project(project)

  renv_remove_rprofile(project)
  renv_remove_rbuildignore(project)

  unlink(renv_paths_local(project), recursive = TRUE)
}


renv_remove_rprofile <- function(project) {

  renv_remove_entry_impl(
    sprintf("source(\"%s\")", renv_paths_local_activate()),
    file.path(project, ".Rprofile")
  )

}

renv_remove_rbuildignore <- function(project) {

  renv_remove_entry_impl(
    renv_paths_local_rbuildignore(),
    file.path(project, ".Rbuildignore")
  )

}

renv_remove_entry_impl <- function(line, file) {

  # if the file doesn't exist, nothing to do
  if (!file.exists(file))
    return(TRUE)

  # if the file doesn't have the line, nothing to do
  contents <- readLines(file, warn = FALSE)
  idx <- grep(line, contents, fixed = TRUE)
  if (!length(idx))
    return(TRUE)

  # remove the line
  contents <- contents[-idx]

  # if the file is now empty, remove it
  if (!length(contents) || all(contents == "")) {
    file.remove(file)
    return(TRUE)
  }

  writeLines(contents, file)
  TRUE

}


