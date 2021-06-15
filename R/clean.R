
#' Clean a Project
#'
#' Clean up a project and its associated \R libraries.
#'
#' @section Actions:
#'
#' The following clean actions are available:
#'
#' \describe{
#'
#' \item{`package.locks`}{
#'
#'   During package installation, \R will create package locks in the
#'   library path, typically named `00LOCK-<package>`. On occasion, if package
#'   installation fails or \R is terminated while installing a package, these
#'   locks can be left behind and will inhibit future attempts to reinstall
#'   that package. Use this action to remove such left-over package locks.
#'
#' }
#'
#' \item{`library.tempdirs`}{
#'
#'   During package installation, \R may create temporary directories with
#'   names of the form `file\w{12}`, and on occasion those files can be
#'   left behind even after they are no longer in use. Use this action to
#'   remove such left-over directories.
#' }
#'
#' \item{`system.library`}{
#'
#'   In general, it is recommended that only packages distributed with \R
#'   are installed into the system library (the library path referred to
#'   by `.Library`). Use this action to remove any user-installed packages
#'   that have been installed to the system library.
#'
#'   Because this action is destructive, it is by default never run -- it
#'   must be explicitly requested by the user.
#'
#' }
#'
#' \item{`unused.packages`}{
#'
#'   Remove packages that are installed in the project library, but no longer
#'   appear to be used in the project sources.
#'
#'   Because this action is destructive, it is by default only run in
#'   interactive sessions when prompting is enabled.
#'
#' }
#'
#' }
#'
#'
#' @inherit renv-params
#'
#' @param actions The set of clean actions to take. See the documentation in
#'   **Actions** for a list of available actions, and the default actions
#'   taken when no actions are supplied.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # clean the current project
#' renv::clean()
#'
#' }
clean <- function(project = NULL,
                  ...,
                  actions = NULL,
                  prompt  = interactive())
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_scope_lock(project = project)

  renv_activate_prompt("clean", NULL, prompt, project)

  renv_dependencies_scope(project, action = "clean")

  actions <- actions %||% renv_clean_actions(prompt)

  all <- list(
    package.locks     = renv_clean_package_locks,
    library.tempdirs  = renv_clean_library_tempdirs,
    system.library    = renv_clean_system_library,
    unused.packages   = renv_clean_unused_packages
  )

  methods <- all[actions]
  for (method in methods)
    tryCatch(method(project, prompt), error = warning)

  vwritef("* The project has been cleaned.")
  invisible(status)
}

renv_clean_actions <- function(prompt) {

  default <- c(
    "package.locks",
    "library.tempdirs"
  )

  unsafe <- c(
    # "system.library",
    "unused.packages"
  )

  c(default, if (prompt) unsafe)

}

renv_clean_library_tempdirs <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No temporary directories were found in the project library.")
    FALSE
  }

  library <- renv_paths_library(project = project)
  children <- list.files(library, full.names = TRUE)

  bad <- grep("/file\\w{12}$", children, value = TRUE)
  if (empty(bad))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    renv_pretty_print(
      bad,
      "The following directories will be removed:",
      wrap = FALSE
    )

    if (prompt && !proceed())
      return(FALSE)

  }
  # nocov end

  unlink(bad, recursive = TRUE)
  TRUE

}


# remove user packages in system library
renv_clean_system_library <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No non-system packages were discovered in the system library.")
    FALSE
  }

  # explicitly query for packages
  syslib <- renv_path_normalize(renv_libpaths_system(), winslash = "/", mustWork = FALSE)
  db <- renv_installed_packages(lib.loc = syslib, priority = "NA")
  packages <- setdiff(db$Package, "translations")

  # also look for leftover package folders
  # (primarily for Windows, where .dlls from old packages can be left behind)

  # nocov start
  if (renv_platform_windows()) {
    folders <- list.files(syslib, full.names = TRUE)
    descpaths <- file.path(folders, "DESCRIPTION")
    missing <- !file.exists(descpaths)
    packages <- union(packages, basename(folders)[missing])
  }
  # nocov end

  # check for any packages needing removal
  if (empty(packages))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    renv_pretty_print(
      packages,
      "The following non-system packages are installed in the system library:",
      c(
        "Normally, only packages distributed with R should be installed in the system library.",
        "These packages will be removed.",
        "If necessary, consider reinstalling these packages in your site library."
      )
    )

    if (prompt && !proceed())
      return(FALSE)

  }
  # nocov end

  remove(packages, library = syslib)
  TRUE

}

renv_clean_unused_packages <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No unused packages were found in the project library.")
    FALSE
  }

  # find packages installed in the project library
  library <- renv_paths_library(project = project)
  installed <- list.files(library)
  if (empty(installed))
    return(ntd())

  # find packages used in the project and their dependencies
  deps <- dependencies(project, progress = FALSE)
  paths <- renv_package_dependencies(deps$Package, project = project)
  packages <- names(paths)

  # figure out which packages aren't needed
  removable <- renv_vector_diff(installed, packages)
  if (empty(removable))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    renv_pretty_print(
      removable,
      c(
        "The following packages are installed in the project library,",
        "but appear to be no longer used in your project."
      ),
      "These packages will be removed."
    )

    if (prompt && !proceed())
      return(FALSE)

  }
  # nocov end

  remove(removable, library = library)
  return(TRUE)

}

renv_clean_package_locks <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No stale package locks were found.")
    FALSE
  }

  # find 00LOCK directories in library
  library <- renv_paths_library(project = project)
  lock <- list.files(path = library, pattern = "^00LOCK", full.names = TRUE)
  if (empty(lock))
    return(ntd())

  # check to see which are old
  now <- Sys.time()
  mtime <- file.mtime(lock)
  mtime[is.na(mtime)] <- now
  diff <- difftime(now, mtime, units = "mins")
  old <- lock[diff > 2]
  if (empty(old))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    renv_pretty_print(
      basename(old),
      "The following stale package locks were discovered in your library:",
      "These locks will be removed.",
      wrap = FALSE
    )

    if (prompt && !proceed())
      return(FALSE)

  }
  # nocov end

  unlink(old, recursive = TRUE)
  TRUE
}

# nocov start
renv_clean_cache <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No unused packages were found in the renv cache.")
    FALSE
  }

  # find projects monitored by renv
  projects <- renv_paths_root("projects")
  projlist <- character()
  if (file.exists(projects))
    projlist <- readLines(projects, warn = FALSE, encoding = "UTF-8")

  # inform user if any projects are missing
  missing <- !file.exists(projlist)
  if (any(missing)) {

    renv_pretty_print(
      projlist[missing],
      "The following projects are monitored by renv, but no longer exist:",
      "These projects will be removed from renv's project list.",
      wrap = FALSE
    )

    if (prompt && !proceed())
      return(FALSE)

    writeLines(projlist[!missing], projects, useBytes = TRUE)

  }

  action <- function(project) {
    library <- renv_paths_library(project = project)
    packages <- list.files(library, full.names = TRUE)
    descs <- file.path(packages, "DESCRIPTION")
    existing <- file.exists(descs)
    map_chr(descs[existing], renv_cache_path, USE.NAMES = FALSE)
  }

  # for each project, find packages used in their renv private library,
  # and look for entries in the cache
  projlist <- projlist[!missing]
  used <- uapply(projlist, renv_progress(action, length(projlist)))

  # check what packages are actually available in the cache
  available <- renv_cache_list()

  diff <- renv_vector_diff(available, used)
  if (empty(diff))
    return(ntd())

  if (prompt || renv_verbose()) {

    renv_pretty_print(
      renv_cache_format_path(diff),
      "The following packages are installed in the cache but no longer used:",
      "These packages will be removed.",
      wrap = FALSE
    )

    if (prompt && !proceed())
      return(FALSE)

  }

  # remove the directories
  unlink(diff, recursive = TRUE)
  renv_cache_clean_empty()
  vwritef("* %i package(s) have been removed.", length(diff))
  TRUE

}
# nocov end
