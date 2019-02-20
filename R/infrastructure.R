
# tools for writing / removing renv-related infrastructure
renv_write_infrastructure <- function(project = NULL) {
  project <- project %||% renv_state$project()

  renv_write_rprofile(project)
  renv_write_rbuildignore(project)
  renv_write_gitignore(project)
  renv_write_activate(project)
  renv_write_project_state(project)
}


renv_write_rprofile <- function(project) {

  renv_write_entry_impl(
    "source(\"renv/activate.R\")",
    file.path(project, ".Rprofile"),
    TRUE
  )

}

renv_write_rbuildignore <- function(project) {

  renv_write_entry_impl(
    "^renv/",
    file.path(project, ".Rbuildignore"),
    FALSE
  )

}

renv_write_gitignore <- function(project) {

  renv_write_entry_impl(
    "renv/library/",
    file.path(project, ".gitignore"),
    FALSE
  )

}

renv_write_activate <- function(project = NULL) {
  project <- project %||% renv_state$project()

  source <- system.file("resources/activate.R", package = "renv")
  target <- file.path(project, "renv/activate.R")

  template <- paste(readLines(source, encoding = "UTF-8"), collapse = "\n")
  new <- sprintf(template, renv_package_version("renv"))

  if (renv_file_exists(target)) {
    old <- readLines(source, warn = FALSE)
    if (identical(old, new))
      return(TRUE)
  }

  ensure_parent_directory(target)
  writeLines(new, con = target)
}

renv_write_project_state <- function(project = NULL) {
  project <- project %||% renv_state$project()

  activate <- file.path(project, "renv/activate.dcf")
  ensure_parent_directory(activate)

  new <- paste("Version:", renv_package_version("renv"))
  if (!renv_file_exists(activate)) {
    writeLines(new, con = activate)
    return(TRUE)
  }

  old <- readLines(activate, warn = FALSE)
  if (identical(old, new))
    return(TRUE)

  writeLines(new, con = activate)
  TRUE
}


renv_write_entry_impl <- function(line, file, force) {

  # check to see if file doesn't exist
  if (!renv_file_exists(file)) {

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
  project <- project %||% renv_state$project()

  renv_remove_rprofile(project)
  renv_remove_rbuildignore(project)
  renv_remove_gitignore(project)

  unlink(file.path(project, "renv"), recursive = TRUE)
}


renv_remove_rprofile <- function(project) {

  renv_remove_entry_impl(
    "source(\"renv/activate.R\")",
    file.path(project, ".Rprofile")
  )

}

renv_remove_rbuildignore <- function(project) {

  renv_remove_entry_impl(
    "^renv/",
    file.path(project, ".Rbuildignore")
  )

}

renv_remove_gitignore <- function(project) {

  renv_remove_entry_impl(
    "renv/library/",
    file.path(project, ".gitignore")
  )

}

renv_remove_entry_impl <- function(line, file) {

  # if the file doesn't exist, nothing to do
  if (!renv_file_exists(file))
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


