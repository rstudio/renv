
# tools for writing / removing renv-related infrastructure
renv_infrastructure_write <- function(project = NULL, version = NULL) {
  project <- project %||% renv_project()

  renv_infrastructure_write_rprofile(project)
  renv_infrastructure_write_rbuildignore(project)
  renv_infrastructure_write_gitignore(project)
  renv_infrastructure_write_activate(project, version = version)
}


renv_infrastructure_write_rprofile <- function(project) {

  renv_infrastructure_write_entry_impl(
    add    = "source(\"renv/activate.R\")",
    remove = character(),
    file   = file.path(project, ".Rprofile"),
    create = TRUE
  )

}

renv_infrastructure_write_rbuildignore <- function(project) {

  lines <- c("^renv$", "^renv\\.lock$")
  if (file.exists(file.path(project, "requirements.txt")))
    lines <- c(lines, "^requirements\\.txt$")
  if (file.exists(file.path(project, "environment.yml")))
    lines <- c(lines, "^environment\\.yml$")

  renv_infrastructure_write_entry_impl(
    add    = lines,
    remove = character(),
    file   = file.path(project, ".Rbuildignore"),
    create = renv_project_type(project) == "package"
  )

}

renv_infrastructure_write_gitignore <- function(project) {

  add    <- stack(mode = "character")
  remove <- stack(mode = "character")

  stk <- if (settings$vcs.ignore.library()) add else remove
  stk$push("library/")

  add$push("python/", "staging/")

  renv_infrastructure_write_entry_impl(
    add    = as.character(add$data()),
    remove = as.character(remove$data()),
    file   = file.path(project, "renv/.gitignore"),
    create = file.exists(file.path(project, ".git"))
  )

}

renv_infrastructure_write_activate <- function(project = NULL, version = NULL) {
  project <- project %||% renv_project()
  version <- version %||% renv_activate_version(project)

  source <- system.file("resources/activate.R", package = "renv")
  target <- file.path(project, "renv/activate.R")

  template <- paste(readLines(source, encoding = "UTF-8"), collapse = "\n")
  new <- renv_template_replace(template, list(VERSION = version))

  if (file.exists(target)) {
    old <- paste(readLines(target, warn = FALSE), collapse = "\n")
    if (old == new)
      return(TRUE)
  }

  ensure_parent_directory(target)
  writeLines(new, con = target)
}


renv_infrastructure_write_entry_impl <- function(add, remove, file, create) {

  # check to see if file doesn't exist
  if (!file.exists(file)) {

    # if we're not forcing file creation, just bail
    if (!create)
      return(TRUE)

    # otherwise, write the file
    ensure_parent_directory(file)
    writeLines(add, file)
    return(TRUE)
  }

  # if the file already has the requested line, nothing to do
  before <- trimws(readLines(file, warn = FALSE))
  after <- before %>% setdiff(remove) %>% union(add)
  if (identical(before, after))
    return(TRUE)

  writeLines(after, file)
  TRUE

}



renv_infrastructure_remove <- function(project = NULL) {
  project <- project %||% renv_project()

  renv_infrastructure_remove_rprofile(project)
  renv_infrastructure_remove_rbuildignore(project)

  unlink(file.path(project, "renv"), recursive = TRUE)
}


renv_infrastructure_remove_rprofile <- function(project) {

  renv_infrastructure_remove_entry_impl(
    "source(\"renv/activate.R\")",
    file.path(project, ".Rprofile")
  )

}

renv_infrastructure_remove_rbuildignore <- function(project) {

  renv_infrastructure_remove_entry_impl(
    "^renv$",
    file.path(project, ".Rbuildignore")
  )

}

renv_infrastructure_remove_entry_impl <- function(line, file) {

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
    unlink(file)
    return(TRUE)
  }

  writeLines(contents, file)
  TRUE

}


