
#' Imbue an renv Installation
#'
#' Imbue an `renv` installation into a project, thereby making the requested
#' version of `renv` available within.
#'
#' Normally, this function does not need to be called directly by the user; it
#' will be invoked as required by [init()] and [activate()].
#'
#' @inherit renv-params
#'
#' @param version The version of `renv` to install. If `NULL`, the version
#'   of `renv` currently installed will be used. The requested version of
#'   `renv` will be retrieved from the `renv` public GitHub repository,
#'   at <https://github.com/rstudio/renv>.
#'
imbue <- function(project = NULL, version = NULL) {
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  vtext <- version %||% renv_package_version("renv")
  vwritef("Installing renv [%s] ...", vtext)
  status <- renv_imbue_impl(project, version)
  vwritef("* Done! renv has been successfully installed.")

  invisible(status)

}

renv_imbue_impl <- function(project, version = NULL, force = FALSE) {

  # don't imbue during tests unless explicitly requested
  if (renv_testing() && !force)
    return(NULL)

  # NULL version means imbue this version of renv
  if (is.null(version))
    return(renv_imbue_self(project))

  # otherwise, try to download and install the requested version
  # of renv from GitHub
  remote <- paste("rstudio/renv", version %||% "master", sep = "@")
  record <- renv_remotes_resolve(remote)
  records <- list(renv = record)

  renv_scope_restore(
    project = project,
    records = records,
    packages = "renv",
    recursive = FALSE
  )

  # retrieve renv
  records <- renv_retrieve("renv")
  record <- records[[1]]

  # ensure renv is installed into project library
  library <- renv_paths_library(project = project)
  ensure_directory(library)
  renv_scope_libpaths(library)

  vwritef("Installing renv [%s] ...", version)
  status <- with(record, r_cmd_install(Package, Path, library))
  vwritef("\tOK [built source]")

  invisible(record)

}

renv_imbue_self <- function(project) {

  # construct source, target paths
  source <- find.package("renv")
  target <- renv_paths_library("renv", project = project)
  if (renv_file_same(source, target))
    return(TRUE)

  type <- renv_package_type(source, quiet = TRUE)
  switch(type,
         source = renv_imbue_self_source(source, target),
         binary = renv_imbue_self_binary(source, target))

  renv_snapshot_description(target)

}

renv_imbue_self_source <- function(source, target) {

  # if the package already exists, just skip
  if (file.exists(target))
    return(TRUE)

  # otherwise, install it
  library <- dirname(target)
  ensure_directory(library)
  r_cmd_install("renv", source, library)

}

renv_imbue_self_binary <- function(source, target) {
  ensure_parent_directory(target)
  renv_file_copy(source, target, overwrite = TRUE)
}
