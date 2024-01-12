
the$init_running <- FALSE

#' Use renv in a project
#'
#' @description
#' Call `renv::init()` to start using renv in the current project. This will:
#'
#' 1. Set up project infrastructure (as described in [scaffold()]) including
#'    the project library and the `.Rprofile` that ensures renv will be
#'    used in all future sessions.
#'
#' 1. Discover the packages that are currently being used in your project and
#'    install them into the project library (as described in [hydrate()]).
#'
#' 1. Create a lockfile that records the state of the project library so it
#'    can be restored by others (as described in [snapshot()]).
#'
#' 1. Restarts R (if running inside RStudio).
#'
#' If you call `init()` on a project that already uses renv, it will attempt
#' to do the right thing: it will restore the project library if it's missing,
#' or otherwise ask you what to do.
#'
#' # Repositories
#'
#' If the default \R repositories have not already been set, renv will use
#' the [Posit Public Package Manager](https://packagemanager.posit.co/) CRAN
#' mirror for package installation. The primary benefit to using this mirror is
#' that it can provide pre-built binaries for \R packages on a variety of
#' commonly-used Linux distributions. This behavior can be configured or
#' disabled if desired -- see the options in [renv::config()] for more details.
#'
#' @inherit renv-params
#'
#' @param project The project directory. When `NULL` (the default), the current
#'   working directory will be used. The \R working directory will be
#'   changed to match the requested project directory.
#'
#' @param settings A list of [settings] to be used with the newly-initialized
#'   project.
#'
#' @param bare Boolean; initialize the project without attempting to discover
#'   and install R package dependencies?
#'
#' @param force Boolean; force initialization? By default, renv will refuse
#'   to initialize the home directory as a project, to defend against accidental
#'   mis-usages of `init()`.
#'
#' @param repos The \R repositories to be used in this project.
#'   See **Repositories** for more details.
#'
#' @param bioconductor The version of Bioconductor to be used with this project.
#'   Setting this may be appropriate if renv is unable to determine that your
#'   project depends on a package normally available from Bioconductor. Set this
#'   to `TRUE` to use the default version of Bioconductor recommended by the
#'   BiocManager package.
#'
#' @param load Boolean; should the project be loaded after it is initialized?
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
                 profile      = NULL,
                 settings     = NULL,
                 bare         = FALSE,
                 force        = FALSE,
                 repos        = NULL,
                 bioconductor = NULL,
                 load         = TRUE,
                 restart      = interactive())
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_scope_binding(the, "init_running", TRUE)

  project <- renv_path_normalize(project %||% getwd())
  renv_project_lock(project = project)

  # initialize profile
  if (!is.null(profile))
    renv_profile_set(profile)

  # normalize repos
  repos <- renv_repos_normalize(repos %||% renv_init_repos())

  # form path to lockfile, library
  library  <- renv_paths_library(project = project)
  lockfile <- renv_lockfile_path(project)

  # ask user what type of project this is
  type <- settings$snapshot.type %||% renv_init_type(project)
  settings$snapshot.type <- type

  # initialize bioconductor pieces
  biocver <- renv_init_bioconductor(bioconductor, project)
  if (!is.null(biocver)) {

    # make sure a Bioconductor package manager is installed
    renv_bioconductor_init(library = library)

    # retrieve bioconductor repositories appropriate for this project
    repos <- renv_bioconductor_repos(project = project, version = biocver)

    # notify user
    writef("- Using Bioconductor version '%s'.", biocver)
    settings[["bioconductor.version"]] <- biocver

  }

  # prepare and move into project directory
  renv_init_validate_project(project, force)
  renv_init_settings(project, settings)

  # for bare inits, just activate the project
  if (bare) {
    renv_imbue_impl(project)
    return(renv_init_fini(project, profile, load, restart))
  }

  # compute and cache dependencies to (a) reveal problems early and (b) compute once
  renv_snapshot_dependencies(project, type = type, dev = TRUE)

  # determine appropriate action
  action <- renv_init_action(project, library, lockfile, bioconductor)
  cancel_if(empty(action) || identical(action, "cancel"))

  # compute library paths for this project
  libpaths <- renv_init_libpaths(project = project)

  # perform the action
  if (action == "init") {
    renv_scope_options(renv.config.dependency.errors = "ignored")
    renv_imbue_impl(project, library = library)
    hydrate(library = library, repos = repos, prompt = FALSE, report = FALSE, project = project)
    snapshot(library = libpaths, repos = repos, prompt = FALSE, project = project, force = TRUE)
  } else if (action == "restore") {
    ensure_directory(library)
    renv_sandbox_activate(project = project)
    restore(project = project, library = libpaths, repos = repos, prompt = FALSE)
  }

  # activate the newly-hydrated project
  renv_init_fini(project, profile, load, restart)

}

renv_init_fini <- function(project, profile, load, restart) {

  renv_activate_impl(
    project = project,
    profile = profile,
    version = renv_metadata_version(),
    load    = load,
    restart = restart
  )

  invisible(project)

}

renv_init_action <- function(project, library, lockfile, bioconductor) {

  # if the user has asked for bioconductor, treat this as a re-initialization
  if (!is.null(bioconductor))
    return("init")

  # figure out appropriate action
  case(

    # if both the library and lockfile exist, ask user for intended action
    file.exists(lockfile)
      ~ renv_init_action_conflict_lockfile(project, library, lockfile),

    # if a private library exists but no lockfile, ask whether we should use it
    file.exists(library)
      ~ renv_init_action_conflict_library(project, library, lockfile),

    # otherwise, we just want to initialize the project
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

  # if the project library exists, but it's empty,
  # treat this as a request to initialize the project
  # https://github.com/rstudio/renv/issues/1668
  db <- installed_packages(lib.loc = library, priority = NA_character_)
  if (nrow(db) == 0L)
    return("init")

  # if only renv is installed, but it matches the version of renv being used
  renvonly <-
    NROW(db) == 1L &&
    db[["Package"]] == "renv" &&
    db[["Version"]] == renv_package_version("renv")

  if (renvonly)
    return("init")

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

  defaults <- renv_settings_get(project)
  merged <- renv_settings_merge(defaults, settings)
  renv_settings_persist(project, merged)
  invisible(merged)

}

renv_init_bioconductor <- function(bioconductor, project) {

  # if we're re-initializing a project that appears to depend
  # on Bioconductor, then use the latest Bioconductor release
  if (is.null(bioconductor)) {
    lockpath <- renv_paths_lockfile(project = project)
    if (file.exists(lockpath)) {
      lockfile <- renv_lockfile_read(lockpath)
      bioconductor <- !is.null(lockfile$Bioconductor)
    }
  }

  # resolve bioconductor argument
  case(
    is.character(bioconductor)     ~ bioconductor,
    identical(bioconductor, TRUE)  ~ renv_bioconductor_version(project, refresh = TRUE),
    identical(bioconductor, FALSE) ~ NULL
  )

}

renv_init_repos <- function(repos = getOption("repos")) {

  # if PPM is disabled, just use default repositories
  repos <- convert(repos, "list")
  if (!renv_ppm_enabled())
    return(repos)

  # check whether the user has opted into using PPM by default
  enabled <- config$ppm.default()
  if (!enabled)
    return(repos)

  # check for default repositories
  #
  # note that if the user is using RStudio, we only want to override
  # the repositories if they haven't explicitly set their own repo URL
  #
  # https://github.com/rstudio/renv/issues/1782
  rstudio <- structure(
    list(CRAN = "https://cran.rstudio.com/"),
    RStudio = TRUE
  )

  isdefault <-
    identical(repos, list(CRAN = "@CRAN@")) ||
    identical(repos, rstudio)

  if (isdefault) {
    repos[["CRAN"]] <- config$ppm.url()
    return(repos)
  }

  # repos appears to have been configured separately; just use it
  repos

}

renv_init_type <- function(project) {

  # check if the user has already requested a snapshot type
  type <- renv_settings_get(project, name = "snapshot.type", default = NULL)
  if (!is.null(type))
    return(type)

  # if we don't have a DESCRIPTION file, use the default
  if (!file.exists(file.path(project, "DESCRIPTION")))
    return(settings$snapshot.type(project = project))

  # otherwise, ask the user if they want to explicitly enumerate their
  # R package dependencies in the DESCRIPTION file
  choice <- menu(

    title = c(
      "This project contains a DESCRIPTION file.",
      "Which files should renv use for dependency discovery in this project?"
    ),

    choices = c(
      explicit = "Use only the DESCRIPTION file. (explicit mode)",
      implicit = "Use all files in this project. (implicit mode)"
    )

  )

  if (identical(choice, "cancel"))
    cancel()

  writef("- Using '%s' snapshot type. Please see `?renv::snapshot` for more details.\n", choice)
  choice

}
