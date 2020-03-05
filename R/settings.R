
`_renv_settings` <- new.env(parent = emptyenv())

renv_settings_default <- function(name) {
  default <- `_renv_settings`[[name]]$default
  renv_options_override("renv.settings", name, default)
}

renv_settings_defaults <- function() {

  keys <- ls(envir = `_renv_settings`, all.names = TRUE)
  vals <- lapply(keys, renv_settings_default)
  names(vals) <- keys
  vals[order(names(vals))]

}

renv_settings_validate <- function(name, value) {

  # NULL implies restore default value
  if (is.null(value))
    return(renv_settings_default(name))

  # otherwise, validate the user-provided value
  validate <- `_renv_settings`[[name]]$validate
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

renv_settings_decode <- function(name, value) {

  # TODO: consider custom decoders per-setting
  decoded <- case(
    value == "NULL"  ~ NULL,
    value == "NA"    ~ NA,
    value == "NaN"   ~ NaN,
    value == "TRUE"  ~ TRUE,
    value == "FALSE" ~ FALSE,
    ~ strsplit(value, "\\s*,\\s*")[[1]]
  )

  renv_settings_validate(name, decoded)

}

renv_settings_read <- function(path) {
  renv_filebacked("settings", path, renv_settings_read_impl)
}

renv_settings_read_impl <- function(path) {

  # check that file exists
  if (!file.exists(path))
    return(NULL)

  # try to read it
  dcf <- catch(renv_dcf_read(path))
  if (inherits(dcf, "error")) {
    warning(dcf)
    return(NULL)
  }

  # keep only known settings
  known <- ls(envir = `_renv_settings`, all.names = TRUE)
  dcf <- keep(dcf, known)

  # decode encoded values
  settings <- enumerate(dcf, renv_settings_decode)

  # merge in defaults
  defaults <- renv_settings_defaults()
  missing <- renv_vector_diff(names(defaults), names(settings))
  settings[missing] <- defaults[missing]

  # and return
  settings

}

renv_settings_get <- function(project, name = NULL) {

  # when 'name' is NULL, return all settings
  if (is.null(name)) {
    names <- ls(envir = `_renv_settings`, all.names = TRUE)
    settings <- lapply(names, renv_settings_get, project = project)
    names(settings) <- names
    return(settings[order(names(settings))])
  }

  # try to read settings file
  path <- renv_settings_path(project)
  settings <- renv_settings_read(path)
  if (!is.null(settings))
    return(settings[[name]])

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
  value <- renv_filebacked_set("settings", path, settings)

  # invoke update callback if value changed
  if (!identical(old, new))
    renv_settings_updated(project, name, old, new)

  # return value
  invisible(value)

}

renv_settings_updated <- function(project, name, old, new) {
  update <- `_renv_settings`[[name]]$update %||% function(...) {}
  update(project, old, new)
}

renv_settings_persist <- function(project, settings) {
  path <- renv_settings_path(project)
  settings <- settings[order(names(settings))]
  settings <- lapply(settings, paste, collapse = ", ")
  ensure_parent_directory(path)
  dcf <- as.data.frame(settings, stringsAsFactors = FALSE)
  renv_dcf_write(dcf, path)
}

renv_settings_merge <- function(settings, merge) {
  settings[names(merge)] <- merge
  settings
}

renv_settings_path <- function(project) {
  file.path(project, "renv/settings.dcf")
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
  targets <- list.files(library, full.names = TRUE)

  sources <- map_chr(targets, renv_cache_path)
  names(targets) <- sources

  if (empty(targets)) {
    fmt <- "* The cache has been %s for this project."
    vwritef(fmt, if (new) "enabled" else "disabled")
    return(TRUE)
  }

  if (new) {
    vprintf("* Copying packages into the cache ... ")
    targets <- targets[file.exists(targets)]
    copy <- renv_progress(renv_cache_move, length(targets))
    enumerate(targets, copy, overwrite = TRUE)
    vwritef("Done!")
  } else {
    vprintf("* Copying packages into the private library ... ")
    targets <- targets[file.exists(sources)]
    unlink(targets)
    copy <- renv_progress(renv_file_copy, length(targets))
    enumerate(targets, copy, overwrite = TRUE)
    vwritef("Done!")
  }

  fmt <- "* The cache has been %s for this project."
  vwritef(fmt, if (new) "enabled" else "disabled")

}

renv_settings_updated_ignore <- function(project, old, new) {
  renv_infrastructure_write_gitignore(project = project)
}



renv_settings_impl <- function(name, validate, default, update) {

  force(name)

  `_renv_settings`[[name]] <- list(
    validate = validate,
    default = default,
    update = update
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

#' Project Settings
#'
#' Define project-local settings that can be used to adjust the behavior of
#' `renv` with your particular project.
#'
#' @section Settings:
#'
#' \describe{
#'
#' \item{\code{external.libraries}}{
#'
#'   A vector of library paths, to be used in addition to the project's own
#'   private library. This can be useful if you have a package available for use
#'   in some global library, but for some reason `renv` is not able to install
#'   that package (e.g. sources or binaries for that package are not publicly
#'   available, or you have been unable to orchestrate the pre-requisites for
#'   installing some packages from source on your machine).
#'
#' }
#'
#' \item{\code{ignored.packages}}{
#'
#'   A vector of packages, which should be ignored when attempting to snapshot
#'   the project's private library. Note that if a package has already been
#'   added to the lockfile, that entry in the lockfile will not be ignored.
#'
#' }
#'
#' \item{\code{package.dependency.fields}}{
#'
#'   During dependency discovery, `renv` uses the fields of an installed
#'   package's `DESCRIPTION` file to determine that package's recursive
#'   dependencies. By default, the `Imports`, `Depends` and `LinkingTo` fields
#'   are used. If you'd prefer that `renv` also captures the `Suggests`
#'   dependencies for a package, you can set this to
#'   `c("Imports", "Depends", "LinkingTo", "Suggests")`.
#'
#' }
#'
#' \item{\code{snapshot.type}}{
#'
#'   The type of snapshot to perform by default. See [snapshot] for more
#'   details.
#'
#' }
#'
#' \item{\code{use.cache}}{
#'
#'   Use a global cache of \R packages. When active, `renv` will install
#'   packages into a global cache, and link packages from the cache into your
#'   `renv` projects as appropriate. This can greatly save on disk space
#'   and install time when for \R packages which are used across multiple
#'   projects in the same environment.
#'
#' }
#'
#' \item{\code{vcs.ignore.library}}{
#'
#'   Set whether the `renv` project library is excluded from version control.
#'
#' }
#'
#' }
#'
#' @section Defaults:
#'
#' You can change the default values of these settings for newly-created `renv`
#' projects by setting \R options for `renv.settings` or `renv.settings.<name>`.
#' For example:
#'
#' \preformatted{
#' options(renv.settings = list(snapshot.type = "all"))
#' options(renv.settings.snapshot.type = "all")
#' }
#'
#' If both of the `renv.settings` and `renv.settings.<name>` options are set
#' for a particular key, the option associated with `renv.settings.<name>` is
#' used instead. We recommend setting these in an appropriate startup profile,
#' e.g. `~/.Rprofile` or similar.
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

  ignored.packages   = renv_settings_impl(
    name     = "ignored.packages",
    validate = is.character,
    default  = character(),
    update   = NULL
  ),

  external.libraries = renv_settings_impl(
    name     = "external.libraries",
    validate = is.character,
    default  = character(),
    update   = NULL
  ),

  package.dependency.fields = renv_settings_impl(
    name     = "package.dependency.fields",
    validate = is.character,
    default  = c("Imports", "Depends", "LinkingTo"),
    update   = NULL
  ),

  snapshot.type = renv_settings_impl(
    name     = "snapshot.type",
    validate = c("all", "custom", "implicit", "explicit", "packrat", "simple"),
    default  = "implicit",
    update   = NULL
  ),

  use.cache = renv_settings_impl(
    name     = "use.cache",
    validate = is.logical,
    default  = TRUE,
    update   = renv_settings_updated_cache
  ),

  vcs.ignore.library = renv_settings_impl(
    name     = "vcs.ignore.library",
    validate = is.logical,
    default  = TRUE,
    update   = renv_settings_updated_ignore
  )

)
