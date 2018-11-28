renv_write_infrastructure <- function(project = ".", name) {
  renv_write_rprofile(project)
  renv_write_rbuildignore(project)
  renv_write_activate(project, name)
  renv_write_active(project, name)
}


renv_write_rprofile <- function(project = ".") {

  renv_write_entry_impl(
    "source('.renv/activate.R')",
    file.path(project, ".Rprofile"),
    TRUE
  )

}

renv_write_rbuildignore <- function(project = ".") {

  renv_write_entry_impl(
    "^\\.renv/",
    file.path(project, ".Rbuildignore"),
    FALSE
  )

}

renv_write_activate <- function(project = ".", name) {

  source <- system.file("resources/activate.R", package = "renv")
  target <- file.path(project, ".renv/activate.R")

  if (file.exists(target)) {
    sc <- readLines(source, warn = FALSE)
    rc <- readLines(target, warn = FALSE)
    if (identical(sc, rc))
      return(TRUE)
  }

  ensure_parent_directory(target)
  file.copy(source, target, overwrite = TRUE)

}

renv_write_active <- function(project = ".", name) {
  active <- file.path(project, ".renv/active")
  ensure_parent_directory(active)

  if (!file.exists(active)) {
    writeLines(name, con = active)
    return(TRUE)
  }

  contents <- readLines(active)
  if (identical(contents, name))
    return(TRUE)

  writeLines(name, con = active)
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



renv_remove_infrastructure <- function(project = ".", name) {
  renv_remove_rprofile(project)
  renv_remove_rbuildignore(project)

  renv <- file.path(project, ".renv")
  unlink(renv, recursive = TRUE)
}


renv_remove_rprofile <- function(project = ".") {

  renv_remove_entry_impl(
    "source('.renv/activate.R')",
    file.path(project, ".Rprofile")
  )

}

renv_remove_rbuildignore <- function(project = ".") {

  renv_remove_entry_impl(
    "^\\.renv/",
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



