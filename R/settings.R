
the$settings <- new.env(parent = emptyenv())

renv_settings_default <- function(name) {
  default <- the$settings[[name]]$default
  renv_options_override("renv.settings", name, default)
}

renv_settings_defaults <- function() {

  keys <- ls(envir = the$settings, all.names = TRUE)
  vals <- lapply(keys, renv_settings_default)
  names(vals) <- keys
  vals[order(names(vals))]

}

renv_settings_validate <- function(name, value) {

  # NULL implies restore default value
  if (is.null(value))
    return(renv_settings_default(name))

  # run coercion method
  value <- the$settings[[name]]$coerce(value)

  # validate the user-provided value
  validate <- the$settings[[name]]$validate
  ok <- case(
    is.character(validate) ~ value %in% validate,
    is.function(validate)  ~ validate(value),
    TRUE
  )

  if (identical(ok, TRUE))
    return(value)

  # validation failed; warn the user and use default
  fmt <- "%s is an invalid value for setting '%s'; using default %s instead"
  default <- renv_settings_default(name)
  warningf(fmt, deparsed(value), name, deparsed(default))
  default

}

renv_settings_read <- function(path) {

  filebacked(
    context  = "renv_settings_read",
    path     = path,
    callback = renv_settings_read_impl
  )

}

renv_settings_read_impl <- function(path) {

  # check that file exists
  if (!file.exists(path))
    return(NULL)

  # read settings
  settings <- case(
    endswith(path, ".dcf")  ~ renv_settings_read_impl_dcf(path),
    endswith(path, ".json") ~ renv_settings_read_impl_json(path),
    ~ stopf("don't know how to read settings file %s", renv_path_pretty(path))
  )

  # keep only known settings
  known <- ls(envir = the$settings, all.names = TRUE)
  settings <- keep(settings, known)

  # validate
  settings <- enumerate(settings, renv_settings_validate)

  # merge in defaults
  defaults <- renv_settings_defaults()
  missing <- renv_vector_diff(names(defaults), names(settings))
  settings[missing] <- defaults[missing]

  # and return
  settings

}

renv_settings_read_impl_dcf <- function(path) {

  # try to read it
  dcf <- catch(renv_dcf_read(path))
  if (inherits(dcf, "error")) {
    warning(dcf)
    return(NULL)
  }

  # decode encoded values
  enumerate(dcf, function(name, value) {

    case(
      value == "NULL"  ~ NULL,
      value == "NA"    ~ NA,
      value == "NaN"   ~ NaN,
      value == "TRUE"  ~ TRUE,
      value == "FALSE" ~ FALSE,
      ~ strsplit(value, "\\s*,\\s*")[[1]]
    )

  })

}

renv_settings_read_impl_json <- function(path) {

  json <- catch(renv_json_read(path))
  if (inherits(json, "error")) {
    warning(json)
    return(NULL)
  }

  json

}

renv_settings_get <- function(project, name = NULL, default = NULL) {

  # when 'name' is NULL, return all settings
  if (is.null(name)) {
    names <- ls(envir = the$settings, all.names = TRUE)
    settings <- lapply(names, renv_settings_get, project = project)
    names(settings) <- names
    return(settings[order(names(settings))])
  }

  # check for an override via option
  override <- renv_options_override("renv.settings", name)
  if (!is.null(override))
    return(override)

  # try to read settings file
  path <- renv_settings_path(project)
  settings <- renv_settings_read(path)
  if (!is.null(settings))
    return(settings[[name]])

  # if a 'default' value was provided, use it
  if (!missing(default))
    return(default)

  # no value recorded; use default
  renv_settings_default(name)

}

renv_settings_set <- function(project, name, value, persist = TRUE) {

  # read old settings
  settings <- renv_settings_get(project)

  # update setting value
  old <- settings[[name]] %||% renv_settings_default(name)
  new <- renv_settings_validate(name, value)
  settings[[name]] <- new

  # persist if requested
  if (persist)
    renv_settings_persist(project, settings)

  # save session-cached value
  path <- renv_settings_path(project)
  value <- renv_filebacked_set("renv_settings_read", path, settings)

  # invoke update callback if value changed
  if (!identical(old, new))
    renv_settings_updated(project, name, old, new)

  # return value
  invisible(value)

}

renv_settings_updated <- function(project, name, old, new) {
  update <- the$settings[[name]]$update %||% function(...) {}
  update(project, old, new)
}

renv_settings_persist <- function(project, settings) {

  path <- renv_settings_path(project)
  settings <- settings[order(names(settings))]

  # figure out which settings are scalar
  scalar <- map_lgl(names(settings), function(name) {
    the$settings[[name]]$scalar
  })

  # use that to determine which objects should be boxed
  config <- renv_json_config(box = names(settings)[!scalar])

  # write json
  ensure_parent_directory(path)
  renv_json_write(
    object = settings,
    config = config,
    file   = path
  )

}

renv_settings_merge <- function(settings, merge) {
  settings[names(merge)] <- merge
  settings
}

renv_settings_path <- function(project) {
  renv_paths_settings(project = project)
}


# nocov start

renv_settings_updated_cache <- function(project, old, new) {

  # if the cache is being disabled, then copy packages from their
  # symlinks back into the library. note that we don't use symlinks
  # on windows (we use hard links) so in that case there's nothing
  # to be done
  if (renv_platform_windows())
    return(FALSE)

  library <- renv_paths_library(project = project)
  pkgpaths <- list.files(library, full.names = TRUE)
  cachepaths <- map_chr(pkgpaths, renv_cache_path)
  names(pkgpaths) <- cachepaths

  if (empty(pkgpaths)) {
    fmt <- "- The cache has been %s for this project."
    writef(fmt, if (new) "enabled" else "disabled")
    return(TRUE)
  }

  printf("- Synchronizing project library with the cache ... ")

  if (new) {

    # enabling the cache: for any package in the project library, replace
    # that copy with a symlink into the cache, moving the associated package
    # into the cache if appropriate

    # ignore existing symlinks; only copy 'real' packages into the cache
    pkgtypes <- renv_file_type(pkgpaths)
    cachepaths <- cachepaths[pkgtypes != "symlink"]

    # move packages from project library into cache
    callback <- renv_progress_callback(renv_cache_move, length(cachepaths))
    enumerate(cachepaths, callback, overwrite = FALSE)

  } else {

    # disabling the cache: for any package which is a symlink into the cache,
    # replace that symlink with a copy of the cached package

    # figure out which package directories are symlinks
    pkgtypes <- renv_file_type(pkgpaths)
    pkgpaths <- pkgpaths[pkgtypes == "symlink"]

    # remove the existing symlinks
    unlink(pkgpaths)

    # overwrite these symlinks with packages from the cache
    callback <- renv_progress_callback(renv_file_copy, length(pkgpaths))
    enumerate(pkgpaths, callback, overwrite = TRUE)

  }

  writef("Done!")

  fmt <- "- The cache has been %s for this project."
  writef(fmt, if (new) "enabled" else "disabled")

}

renv_settings_updated_ignore <- function(project, old, new) {
  renv_infrastructure_write_gitignore(project = project)
}

renv_settings_migrate <- function(project) {

  old <- renv_paths_renv("settings.dcf",  project = project)
  if (!file.exists(old))
    return()

  new <- renv_paths_renv("settings.json", project = project)
  if (file.exists(new))
    return()

  # update settings
  settings <- renv_settings_read(old)
  renv_settings_persist(project, settings)

}

renv_settings_impl <- function(name, default, scalar, validate, coerce, update) {

  force(name)

  the$settings[[name]] <- list(
    default  = default,
    coerce   = coerce,
    scalar   = scalar,
    validate = validate,
    update   = update
  )

  function(value, project = NULL, persist = TRUE) {
    project <- renv_project_resolve(project)
    if (missing(value))
      renv_settings_get(project, name)
    else
      renv_settings_set(project, name, value, persist)
  }

}


# nocov end

#' Project settings
#'
#' @description
#' Define project-local settings that can be used to adjust the behavior of
#' renv with your particular project.
#'
#' * Get the current value of a setting with (e.g.) `settings$snapshot.type()`
#' * Set current value of a setting with (e.g.)
#'   `settings$snapshot.type("explicit")`.
#'
#' Settings are automatically persisted across project sessions by writing to
#' `renv/settings.json`. You can also edit this file by hand, but you'll need
#' to restart the session for those changes to take effect.
#'
#' ## `bioconductor.version`
#'
#' The Bioconductor version to be used with this project. Use this if you'd
#' like to lock the version of Bioconductor used on a per-project basis.
#' When unset, renv will try to infer the appropriate Bioconductor release
#' using the BiocVersion package if installed; if not, renv uses
#' `BiocManager::version()` to infer the appropriate Bioconductor version.
#'
#' ## `external.libraries`
#'
#' A vector of library paths, to be used in addition to the project's own
#' private library. This can be useful if you have a package available for use
#' in some system library, but for some reason renv is not able to install
#' that package (e.g. sources or binaries for that package are not publicly
#' available, or you have been unable to orchestrate the pre-requisites for
#' installing some packages from source on your machine).
#'
#' ## `ignored.packages`
#'
#' A vector of packages, which should be ignored when attempting to snapshot
#' the project's private library. Note that if a package has already been
#' added to the lockfile, that entry in the lockfile will not be ignored.
#'
#' ## `package.dependency.fields`
#'
#' When installing a package with `install()`, what `DESCRIPTION` fields should
#' be used to determine that package's dependencies? The default uses
#' `c("Imports", "Depends", "LinkingTo")`, but if you also want  to install
#' `Suggests` dependencies for a package, you can set this to
#' `c("Imports", "Depends", "LinkingTo", "Suggests")`.
#'
#' ## `ppm.enabled`
#'
#' Enable [Posit Package Manager](https://packagemanager.posit.co/)
#' integration in this project? When `TRUE`, renv will attempt to transform
#' repository URLs used by PPM into binary URLs as appropriate for the
#' current Linux platform. Set this to `FALSE` if you'd like to continue using
#' source-only PPM URLs, or if you find that renv is improperly transforming
#' your repository URLs. You can still set and use PPM repositories with this
#' option disabled; it only controls whether renv tries to transform source
#' repository URLs into binary URLs on your behalf.
#'
#' ## `ppm.ignored.urls`
#'
#' When [Posit Package Manager](https://packagemanager.posit.co/) integration
#' is enabled, `renv` will attempt to transform source repository URLs into
#' binary repository URLs. This setting can be used if you'd like to avoid this
#' transformation with some subset of repository URLs.
#'
#' ## `r.version`
#'
#' The version of \R to encode within the lockfile. This can be set as a
#' project-specific option if you'd like to allow multiple users to use
#' the same renv project with different versions of \R. renv will
#' still warn the user if the major + minor version of \R used in a project
#' does not match what is encoded in the lockfile.
#'
#' ## `snapshot.type`
#'
#' The type of snapshot to perform by default. See [snapshot] for more
#' details.
#'
#' ## `use.cache`
#'
#' Enable the renv package cache with this project. When active, renv will
#' install packages into a global cache, and link packages from the cache into
#' your renv projects as appropriate. This can greatly save on disk space
#' and install time when for \R packages which are used across multiple
#' projects in the same environment.
#'
#' ## `vcs.manage.ignores`
#'
#' Should renv attempt to manage the version control system's ignore files
#' (e.g. `.gitignore`) within this project? Set this to `FALSE` if you'd
#' prefer to take control. Note that if this setting is enabled, you will
#' need to manually ensure internal data in the project's `renv/` folder
#' is explicitly ignored.
#'
#' ## `vcs.ignore.cellar`
#'
#' Set whether packages within a project-local package cellar are excluded
#' from version control. See `vignette("package-sources", package = "renv")`
#' for more information.
#'
#' ## `vcs.ignore.library`
#'
#' Set whether the renv project library is excluded from version control.
#'
#' ## `vcs.ignore.local`
#'
#' Set whether renv project-specific local sources are excluded from version
#' control.
#'
#' # Defaults
#'
#' You can change the default values of these settings for newly-created renv
#' projects by setting \R options for `renv.settings` or `renv.settings.<name>`.
#' For example:
#'
#' ```R
#' options(renv.settings = list(snapshot.type = "all"))
#' options(renv.settings.snapshot.type = "all")
#' ```
#'
#' If both of the `renv.settings` and `renv.settings.<name>` options are set
#' for a particular key, the option associated with `renv.settings.<name>` is
#' used instead. We recommend setting these in an appropriate startup profile,
#' e.g. `~/.Rprofile` or similar.
#'
#' @return
#'   A named list of renv settings.
#'
#' @format NULL
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # view currently-ignored packaged
#' renv::settings$ignored.packages()
#'
#' # ignore a set of packages
#' renv::settings$ignored.packages("devtools", persist = FALSE)
#'
#' }
settings <- list(

  bioconductor.version = renv_settings_impl(
    name     = "bioconductor.version",
    default  = NULL,
    scalar   = TRUE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  ignored.packages = renv_settings_impl(
    name     = "ignored.packages",
    default  = character(),
    scalar   = FALSE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  external.libraries = renv_settings_impl(
    name     = "external.libraries",
    default  = character(),
    scalar   = FALSE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  package.dependency.fields = renv_settings_impl(
    name     = "package.dependency.fields",
    default  = c("Imports", "Depends", "LinkingTo"),
    scalar   = FALSE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  ppm.enabled = renv_settings_impl(
    name     = "ppm.enabled",
    default  = NULL,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = NULL
  ),

  ppm.ignored.urls = renv_settings_impl(
    name     = "ppm.ignored.urls",
    default  = NULL,
    scalar   = FALSE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  r.version = renv_settings_impl(
    name     = "r.version",
    default  = NULL,
    scalar   = TRUE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  snapshot.type = renv_settings_impl(
    name     = "snapshot.type",
    default  = "implicit",
    scalar   = TRUE,
    validate = c("all", "custom", "implicit", "explicit", "packrat", "simple"),
    coerce   = as.character,
    update   = NULL
  ),

  use.cache = renv_settings_impl(
    name     = "use.cache",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = renv_settings_updated_cache
  ),

  vcs.manage.ignores = renv_settings_impl(
    name     = "vcs.manage.ignores",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = NULL
  ),

  vcs.ignore.cellar = renv_settings_impl(
    name     = "vcs.ignore.cellar",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = renv_settings_updated_ignore
  ),

  vcs.ignore.library = renv_settings_impl(
    name     = "vcs.ignore.library",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = renv_settings_updated_ignore
  ),

  vcs.ignore.local = renv_settings_impl(
    name     = "vcs.ignore.local",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = renv_settings_updated_ignore
  )

)
