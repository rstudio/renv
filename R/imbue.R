
#' Imbue an renv Installation
#'
#' Imbue an renv installation into a project, thereby making the requested
#' version of renv available within.
#'
#' Normally, this function does not need to be called directly by the user; it
#' will be invoked as required by [init()] and [activate()].
#'
#' @inherit renv-params
#'
#' @param version The version of renv to install. If `NULL`, the version
#'   of renv currently installed will be used. The requested version of
#'   renv will be retrieved from the renv public GitHub repository,
#'   at <https://github.com/rstudio/renv>.
#'
#' @param quiet Boolean; avoid printing output during install of renv?
#'
imbue <- function(project = NULL,
                  version = NULL,
                  quiet   = FALSE)
{
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  renv_scope_options(renv.verbose = !quiet)

  vtext <- version %||% renv_metadata_version()
  writef("Installing renv [%s] ...", vtext)
  status <- renv_imbue_impl(project, version)
  writef("- Done! renv has been successfully installed.")

  invisible(status)

}

renv_imbue_impl <- function(project,
                            library = NULL,
                            version = NULL,
                            force = FALSE)
{
  # don't imbue during tests unless explicitly requested
  if (renv_tests_running() && !force)
    return(NULL)

  # resolve library path
  library <- library %||% renv_paths_library(project = project)
  ensure_directory(library)

  # NULL version means imbue this version of renv
  if (is.null(version))
    return(renv_imbue_self(project, library = library))

  # otherwise, try to download and install the requested version
  # of renv from GitHub
  remote <- paste("rstudio/renv", version %||% "main", sep = "@")
  record <- renv_remotes_resolve(remote)
  records <- list(renv = record)

  renv_scope_restore(
    project = project,
    library = library,
    records = records,
    packages = "renv",
    recursive = FALSE
  )

  records <- renv_retrieve_impl("renv")
  renv_install_impl(records)

  record <- records[["renv"]]
  invisible(record)
}

renv_imbue_self <- function(project,
                            library = NULL,
                            source = NULL)
{
  # construct source, target paths
  # (check if 'renv' is loaded to handle embedded case)
  source <- source %||% {
    if ("renv" %in% loadedNamespaces()) {
      renv_namespace_path("renv")
    } else {
      renv_package_find("renv")
    }
  }

  if (!file.exists(source))
    stop("internal error: could not find where 'renv' is installed")

  library <- library %||% renv_paths_library(project = project)
  target <- file.path(library, "renv")
  if (renv_file_same(source, target))
    return(TRUE)

  type <- renv_package_type(source, quiet = TRUE)
  case(
    type == "source" ~ renv_imbue_self_source(source, target),
    type == "binary" ~ renv_imbue_self_binary(source, target)
  )

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
