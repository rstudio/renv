
# tools for writing / removing renv-related infrastructure
renv_infrastructure_write <- function(project = NULL,
                                      profile = NULL,
                                      version = NULL)
{
  project <- renv_project_resolve(project)

  renv_infrastructure_write_profile(project, profile = profile)
  renv_infrastructure_write_rprofile(project)
  renv_infrastructure_write_rbuildignore(project)
  renv_infrastructure_write_gitignore(project)
  renv_infrastructure_write_activate(project, version = version)
}

renv_infrastructure_write_profile <- function(project, profile = NULL) {

  path <- renv_paths_renv("profile", profile = FALSE, project = project)
  profile <- renv_profile_normalize(profile)
  if (is.null(profile))
    return(unlink(path))

  ensure_parent_directory(path)
  writeLines(profile, con = path)

}

renv_infrastructure_write_rprofile <- function(project) {

  if (!config$autoloader.enabled())
    return()

  # NOTE: intentionally leave project NULL to compute relative path
  path <- renv_paths_activate(project = NULL)
  add <- sprintf("source(%s)", renv_json_quote(path))

  renv_infrastructure_write_entry_impl(
    add    = add,
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

  stk <- if (settings$vcs.ignore.local()) add else remove
  stk$push("local/")

  stk <- if (settings$vcs.ignore.cellar()) add else remove
  stk$push("cellar/")

  add$push("lock/", "python/", "staging/")

  renv_infrastructure_write_entry_impl(
    add    = as.character(add$data()),
    remove = as.character(remove$data()),
    file   = renv_paths_renv(".gitignore", project = project),
    create = TRUE
  )

}

renv_infrastructure_write_activate <- function(project = NULL,
                                               version = NULL,
                                               create  = TRUE)
{
  project <- renv_project_resolve(project)
  version <- version %||% renv_activate_version(project)

  source <- system.file("resources/activate.R", package = "renv")
  target <- renv_paths_activate(project = project)

  if (!create && !file.exists(target))
    return(FALSE)

  template <- renv_file_read(source)
  new <- renv_template_replace(template, list(VERSION = version))

  if (file.exists(target)) {
    old <- renv_file_read(target)
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
  before <- readLines(file, warn = FALSE)
  after <- before

  # add requested entries
  for (item in rev(add)) {

    # check to see if the requested line exists (either commented
    # or uncommented). if it exists, we'll attempt to uncomment
    # any commented lines
    cpattern <- sprintf("^\\s*#?\\s*\\Q%s\\E\\s*(?:#|\\s*$)", item)
    matches <- grepl(cpattern, after, perl = TRUE)
    if (any(matches))
      after[matches] <- gsub("^(\\s*)#\\s*", "\\1", after[matches])
    else
      after <- c(item, after)

  }

  # remove requested entries
  for (item in rev(remove)) {
    pattern <- sprintf("^\\s*\\Q%s\\E\\s*(?:#|\\s*$)", item)
    matches <- grepl(pattern, after, perl = TRUE)
    if (any(matches)) {
      replacement <- gsub("^(\\s*)", "\\1# ", after[matches], perl = TRUE)
      after[matches] <- replacement
    }
  }

  # write to file if we have changes
  if (!identical(before, after))
    writeLines(after, file)

  TRUE

}



renv_infrastructure_remove <- function(project = NULL) {
  project <- renv_project_resolve(project)

  renv_infrastructure_remove_rprofile(project)
  renv_infrastructure_remove_rbuildignore(project)

  unlink(file.path(project, "renv"), recursive = TRUE)
}


renv_infrastructure_remove_rprofile <- function(project) {

  # NOTE: intentionally leave project NULL to compute relative path
  path <- renv_paths_activate(project = NULL)
  line <- sprintf("source(%s)", renv_json_quote(path))

  renv_infrastructure_remove_entry_impl(
    line      = line,
    file      = file.path(project, ".Rprofile"),
    removable = TRUE
  )

}

renv_infrastructure_remove_rbuildignore <- function(project) {

  renv_infrastructure_remove_entry_impl(
    line      = "^renv$",
    file      = file.path(project, ".Rbuildignore"),
    removable = FALSE
  )

}

renv_infrastructure_remove_entry_impl <- function(line, file, removable) {

  # if the file doesn't exist, nothing to do
  if (!file.exists(file))
    return(TRUE)

  # find and comment out the line
  contents <- readLines(file, warn = FALSE)
  pattern <- sprintf("^\\s*\\Q%s\\E\\s*(?:#|\\s*$)", line)
  matches <- grepl(pattern, contents, perl = TRUE)

  # if this file is removable, check to see if we matched all non-blank
  # lines; if so, remove the file
  if (removable) {
    rest <- contents[!matches]
    if (all(grepl("^\\s*$", rest)))
      return(unlink(file))
  }

  # otherwise, just mutate the file
  replacement <- gsub("^(\\s*)", "\\1# ", contents[matches], perl = TRUE)
  contents[matches] <- replacement
  writeLines(contents, file)

  TRUE

}


