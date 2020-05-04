
#' Initialize a Project
#'
#' Discover packages used within the current project, and then initialize a
#' project-local private \R library with those packages. The currently-installed
#' versions of any packages in use (as detected within the default R libraries)
#' are then installed to the project's private library.
#'
#' The primary steps taken when initializing a new project are:
#'
#' 1. \R package dependencies are discovered within the \R files used within
#'    the project with [dependencies()];
#'
#' 2. Discovered packages are copied into the `renv` global package cache, so
#'    these packages can be re-used across future projects as necessary;
#'
#' 3. Any missing \R package dependencies discovered are then installed into
#'    the project's private library;
#'
#' 4. A lockfile capturing the state of the project's library is created
#'    with [snapshot()];
#'
#' 5. The project is activated with [activate()].
#'
#' This mimics the workflow provided by `packrat::init()`, but with a few
#' differences -- in particular, `renv` does not attempt to download and store
#' package sources, and `renv` will re-use packages that have already been
#' installed whenever possible.
#'
#' If `renv` sees that the associated project has already been initialized and
#' has a lockfile, then it will attempt to infer the appropriate action to take
#' based on the presence of a private library. If no library is available,
#' `renv` will restore the private library from the lockfile; if one is
#' available, `renv` will ask if you want to perform a 'standard' init,
#' restore from the lockfile, or activate the project without taking any
#' further action.
#'
#' @section Infrastructure:
#'
#' `renv` will write or amend the following files in the project:
#'
#' - `.Rprofile`: An auto-loader will be installed, so that new R sessions
#'   launched within the project are automatically loaded.
#'
#' - `renv/activate.R`: This script is run by the previously-mentioned
#'   `.Rprofile` to load the project.
#'
#' - `renv/.gitignore`: This is used to instruct Git to ignore the project's
#'   private library, as it should normally not be committed to a version
#'   control repository.
#'
#' - `.Rbuildignore`: to ensure that the `renv` directory is ignored during
#'   package development; e.g. when attempting to build or install a package
#'   using `renv`.
#'
#' @inherit renv-params
#'
#' @param project The project directory. The \R working directory will be
#'   changed to match the requested project directory.
#'
#' @param settings A list of [settings] to be used with the newly-initialized
#'   project.
#'
#' @param bare Boolean; initialize the project without attempting to discover
#'   and install R package dependencies?
#'
#' @param force Boolean; force initialization? By default, `renv` will refuse
#'   to initialize the home directory as a project, to defend against accidental
#'   mis-usages of `init()`.
#'
#' @param restart Boolean; attempt to restart the \R session after initializing
#'   the project? A session restart will be attempted if the `"restart"` \R
#'   option is set by the frontend embedding \R.
#'
#' @export
#'
#' @example examples/examples-init.R
init <- function(project = NULL,
                 ...,
                 settings = NULL,
                 bare     = FALSE,
                 force    = FALSE,
                 restart  = interactive())
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- project %||% getwd()

  # prepare and move into project directory
  renv_init_validate_project(project, force)
  renv_init_settings(project, settings)
  setwd(project)

  # collect dependencies
  renv_dependencies_scope(project, action = "init")

  # be quiet in RStudio projects (as we will normally restart automatically)
  quiet <- !is.null(getOption("restart"))

  # for bare inits, just active the project
  if (bare) {
    version <- renv_package_version("renv")
    status <- renv_activate_impl(project, version, restart, quiet)
    return(invisible(status))
  }

  # form path to lockfile, library
  library  <- renv_paths_library(project = project)
  lockfile <- renv_lockfile_path(project)

  # determine appropriate action
  action <- renv_init_action(project, library, lockfile)
  if (empty(action) || identical(action, "cancel")) {
    message("* Operation aborted.")
    return(invisible(FALSE))
  }

  # perform the action
  if (action == "init") {
    vwritef("* Initializing project ...")
    renv_libpaths_activate(project = project)
    renv_imbue_impl(project)
    hydrate(project = project, library = library)
    snapshot(project = project, library = library, prompt = FALSE)
  } else if (action == "restore") {
    vwritef("* Restoring project ... ")
    ensure_directory(library)
    restore(project = project, library = library, prompt = FALSE)
  }

  # activate the newly-hydrated project
  version <- renv_package_version("renv")
  status <- renv_activate_impl(project, version, restart, quiet)
  invisible(project)

}

renv_init_action <- function(project, library, lockfile) {

  # figure out appropriate action
  case(

    # if both the library and lockfile exist, ask user for intended action
    file.exists(lockfile) && file.exists(library)
      ~ renv_init_action_conflict_lockfile(project, library, lockfile),

    # if a private library exists but no lockfile, ask whether we should use it
    !file.exists(lockfile) && file.exists(library)
      ~ renv_init_action_conflict_library(project, library, lockfile),

    # if a lockfile exists but not a library, we just want to restore
    file.exists(lockfile) && !file.exists(library)
      ~ "restore",

    # otherwise, we juse want to initialize the project
    ~ "init"

  )

}

renv_init_action_conflict_lockfile <- function(project, library, lockfile) {

  if (!interactive())
    return("nothing")

  title <- "This project already has a lockfile. What would you like to do?"
  choices <- c(
    restore = "Restore the project from the lockfile.",
    init    = "Discard the lockfile and re-initialize the project.",
    nothing = "Activate the project without snapshotting or installing any packages.",
    cancel  = "Abort project initialization."
  )

  selection <- tryCatch(
    utils::select.list(choices, title = title, graphics = FALSE),
    interrupt = identity
  )

  if (inherits(selection, "interrupt"))
    return(NULL)

  names(selection)

}

renv_init_action_conflict_library <- function(project, library, lockfile) {

  if (!interactive())
    return("nothing")

  title <- "This project already has a private library. What would you like to do?"
  choices <- c(
    nothing = "Activate the project and use the existing library.",
    init    = "Re-initialize the project with a new library.",
    cancel  = "Abort project initialization."
  )

  selection <- tryCatch(
    utils::select.list(choices, title = title, graphics = FALSE),
    interrupt = identity
  )

  if (inherits(selection, "interrupt"))
    return(NULL)

  names(selection)

}

renv_init_validate_project <- function(project, force) {

  # allow all project directories when force = TRUE
  if (force)
    return(TRUE)

  # disallow attempts to initialize renv in the home directory
  home <- path.expand("~/")
  msg <- if (renv_file_same(project, home))
    "refusing to initialize project in home directory"
  else if (renv_path_within(home, project))
    sprintf("refusing to initialize project in directory '%s'", project)

  if (!is.null(msg)) {
    msg <- paste(msg, "-- use renv::init(force = TRUE) to override")
    stopf(msg)
  }

}

renv_init_settings <- function(project, settings) {

  ensure_directory(file.path(project, "renv"))
  defaults <- renv_settings_get(project)
  merged <- renv_settings_merge(defaults, settings)
  renv_settings_persist(project, merged)
  invisible(merged)

}
