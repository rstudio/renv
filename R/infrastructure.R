
# tools for writing / removing renv-related infrastructure
renv_write_infrastructure <- function(project = NULL, version = NULL) {
  project <- project %||% renv_project()

  renv_write_rprofile(project)
  renv_write_rbuildignore(project)
  renv_write_gitignore(project)
  renv_write_activate(project, version = version)
}


renv_write_rprofile <- function(project) {

  renv_write_entry_impl(
    lines  = "source(\"renv/activate.R\")",
    file   = file.path(project, ".Rprofile"),
    create = TRUE
  )

}

renv_write_rbuildignore <- function(project) {

  renv_write_entry_impl(
    lines  = "^renv/",
    file   = file.path(project, ".Rbuildignore"),
    create = file.exists(file.path(project, "DESCRIPTION"))
  )

}

renv_write_gitignore <- function(project) {

  renv_write_entry_impl(
    lines  = c("library/", "r-reticulate/"),
    file   = file.path(project, "renv/.gitignore"),
    create = file.exists(file.path(project, ".git"))
  )

}

renv_write_activate <- function(project = NULL, version = NULL) {
  project <- project %||% renv_project()
  version <- version %||% renv_package_version("renv")

  source <- system.file("resources/activate.R", package = "renv")
  target <- file.path(project, "renv/activate.R")

  template <- paste(readLines(source, encoding = "UTF-8"), collapse = "\n")
  new <- sprintf(template, version)

  if (renv_file_exists(target)) {
    old <- readLines(target, warn = FALSE)
    if (identical(old, new))
      return(TRUE)
  }

  ensure_parent_directory(target)
  writeLines(new, con = target)
}


renv_write_entry_impl <- function(lines, file, create) {

  # check to see if file doesn't exist
  if (!renv_file_exists(file)) {

    # if we're not forcing file creation, just bail
    if (!create)
      return(TRUE)

    # otherwise, write the file
    ensure_parent_directory(file)
    writeLines(lines, file)
    return(TRUE)
  }

  # if the file already has the requested line, nothing to do
  contents <- trimws(readLines(file, warn = FALSE))
  missing <- setdiff(lines, contents)
  if (empty(missing))
    return(TRUE)

  amended <- c(contents, missing)
  writeLines(amended, file)
  TRUE

}



renv_remove_infrastructure <- function(project = NULL) {
  project <- project %||% renv_project()

  renv_remove_rprofile(project)
  renv_remove_rbuildignore(project)

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


