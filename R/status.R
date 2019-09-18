
#' Status
#'
#' Report differences between the project's lockfile and the current state of
#' the private library (if any).
#'
#' @inherit renv-params
#'
#' @param library The path to a library. By default, the project library
#'   associated with the requested project `project` is used.
#'
#' @param lockfile The path to a lockfile. By default, the project lockfile
#'   (called `renv.lock`) is used.
#'
#' @return This function is normally called for its side effects.
#'
#' @export
#'
#' @example examples/examples-init.R
status <- function(project = NULL,
                   ...,
                   library = NULL,
                   lockfile = NULL)
{
  renv_scope_error_handler()
  project <- project %||% renv_project()
  library <- library %||% renv_libpaths_all()
  lockfile <- lockfile %||% renv_lockfile_path(project)
  invisible(renv_status(project, library, lockfile))
}

renv_status <- function(project, library, lockfile) {

  # check to see if we've initialized this project
  if (!renv_project_initialized(project)) {
    vwritef("* This project has not yet been initialized.")
    return(FALSE)
  }

  # report missing lockfile
  if (!file.exists(lockfile)) {
    text <- if (identical(lockfile, renv_lockfile_path(project)))
      "* This project has not yet been snapshotted -- 'renv.lock' does not exist."
    else
      sprintf("* Lockfile '%s' does not exist.", aliased_path(lockfile))
    vwritef(text)
  }

  # report missing library
  projlib <- library[[1]]
  if (!file.exists(projlib)) {
    text <- if (identical(projlib, renv_paths_library(project = project)))
      "* This project's private library is empty or does not exist."
    else
      sprintf("* Library '%s' is empty or does not exist.", aliased_path(projlib))
    vwritef(text)
  }

  # report status of cache
  if (settings$use.cache(project = project))
    renv_cache_diagnose()

  # compare the lockfile with current state of library
  curr <- local({
    renv_scope_options(renv.verbose = FALSE)
    snapshot(project = project, library = library, lockfile = NULL, force = TRUE)
  })

  lock <- renv_lockfile_load(project)
  renv_status_report(lock, curr)
  list(library = curr, lockfile = lock)

}

renv_status_report <- function(lock, curr) {

  actions <- renv_lockfile_diff_packages(lock, curr)
  if (empty(actions))
    vwritef("* The project is already synchronized with the lockfile.")

  if ("install" %in% actions) {
    renv_pretty_print_records(
      renv_records_select(curr, actions, "install"),
      "The following package(s) are installed but not recorded in the lockfile:",
      "Use `renv::snapshot()` to add these packages to your lockfile."
    )
  }

  if ("remove" %in% actions) {
    renv_pretty_print_records(
      renv_records_select(lock, actions, "remove"),
      "The following package(s) are recorded in the lockfile but not installed:",
      "Use `renv::restore()` to install these packages."
    )
  }

  rest <- c("upgrade", "downgrade", "crossgrade")
  if (any(rest %in% actions)) {

    rlock <- renv_records(lock)
    rcurr <- renv_records(curr)

    matches <- actions[actions %in% rest]
    data <- data.frame(
      "  Package"          = names(matches),
      "  Lockfile Version" = extract_chr(rlock[names(matches)], "Version"),
      "  Library Version"  = extract_chr(rcurr[names(matches)], "Version"),
      row.names            = NULL,
      stringsAsFactors     = FALSE,
      check.names          = FALSE
    )

    writeLines("The following package(s) are out of sync:")
    writeLines("")
    print(data, row.names = FALSE)
    writeLines("")
    writeLines("Use `renv::snapshot()` to save the state of your library to the lockfile.")
    writeLines("Use `renv::restore()` to restore your library from the state of the lockfile.")
    writeLines("")
  }

}
