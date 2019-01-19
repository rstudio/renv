
# TODO: allow for e.g. `~/.renvignore`?
renv_renvignore_get <- function(project = NULL) {

  # read defaults
  defaults <- renv_renvignore_defaults()

  # construct path to '.renvignore'
  project <- project %||% renv_state$project()
  path <- file.path(project, ".renvignore")
  if (!renv_file_exists(path))
    return(defaults)

  # return ignores
  ignores <- renv_filebacked_get(path) %||% renv_renvignore_read(path)
  c(ignores, defaults)

}

# TODO: parse things like '**' and so on
renv_renvignore_read <- function(path) {
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")
  matches <- grep("^\\s*(?:#|\\s*$)", contents, value = TRUE, invert = TRUE)
  renv_filebacked_set(path, matches)
}

renv_renvignore_defaults <- function() {
  c("node_modules", "packrat", "revdep", "renv/library", "renv/bootstrap")
}
