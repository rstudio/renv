
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
  if (validate(value))
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

renv_settings_read <- function(project) {

  path <- file.path(project, "renv/settings.dcf")
  if (!file.exists(path))
    return(renv_settings_defaults())

  dcf <- catch(renv_dcf_read(path))
  if (inherits(dcf, "error")) {
    warning(dcf)
    return(renv_settings_defaults())
  }

  known <- ls(envir = `_renv_settings`, all.names = TRUE)
  dcf <- dcf[intersect(names(dcf), known)]

  settings <- enumerate(dcf, renv_settings_decode)

  defaults <- renv_settings_defaults()
  missing <- setdiff(names(defaults), names(settings))
  settings[missing] <- defaults[missing]

  renv_filebacked_set("settings", path, settings)

}

renv_settings_get <- function(project, name = NULL) {

  # check for a cached settings value
  path <- file.path(project, "renv/settings.dcf")
  cache <- renv_filebacked_get("settings", path)
  if (!is.null(cache))
    return(if (is.null(name)) cache else cache[[name]])

  # no cache; read the settings file and check
  settings <- renv_settings_read(project)
  if (is.null(name))
    return(settings)

  # get requested setting
  setting <- settings[[name]]
  if (!is.null(setting))
    return(setting)

  # no value recorded; check for global user config
  config <- renv_config(name)
  if (!is.null(config))
    return(config)

  # no user-defined value available; use default
  renv_settings_default(name)

}

renv_settings_set <- function(project, name, value, persist = TRUE) {

  path <- file.path(project, "renv/settings.dcf")

  settings <-
    renv_filebacked_get("settings", path) %||%
    renv_settings_read(project)

  old <- settings[[name]] %||% renv_settings_default(name)
  new <- renv_settings_validate(name, value)
  settings[[name]] <- new

  if (!identical(old, new))
    renv_settings_updated(project, name, old, new)

  renv_filebacked_set("settings", path, settings)

  if (persist)
    renv_settings_persist(project, settings)

}

renv_settings_updated <- function(project, name, old, new) {
  update <- `_renv_settings`[[name]]$update %||% function(...) {}
  update(project, old, new)
}

renv_settings_persist <- function(project, settings) {
  path <- file.path(project, "renv/settings.dcf")
  settings <- settings[order(names(settings))]
  settings <- lapply(settings, paste, collapse = ", ")
  ensure_parent_directory(path)
  renv_dcf_write(as.data.frame(settings, stringsAsFactors = FALSE), path)
}

renv_settings_merge <- function(settings, merge) {
  settings[names(merge)] <- merge
  settings
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

  sources <- map_chr(targets, function(target) {
    record <- renv_description_read(target)
    record$Hash <- renv_hash_description(target)
    renv_cache_package_path(record)
  })

  names(targets) <- sources

  if (empty(targets)) {
    fmt <- "* The cache has been %s for this project."
    vwritef(fmt, if (new) "enabled" else "disabled")
    return(TRUE)
  }

  if (new) {
    vprintf("* Copying packages into the cache ... ")
    copy <- renv_progress(renv_cache_move, length(targets))
    enumerate(targets, copy, overwrite = TRUE)
    vwritef("Done!")
  } else {
    vprintf("* Copying packages into the private library ... ")
    unlink(targets)
    copy <- renv_progress(renv_file_copy, length(targets))
    enumerate(targets, copy, overwrite = TRUE)
    vwritef("Done!")
  }

  fmt <- "* The cache has been %s for this project."
  vwritef(fmt, if (new) "enabled" else "disabled")

}



renv_settings_impl <- function(name, validate, default, update) {

  force(name)

  `_renv_settings`[[name]] <- list(
    validate = validate,
    default = default,
    update = update
  )

  function(value, project = NULL, persist = TRUE) {
    project <- project %||% renv_project()
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
#' \item{\code{ignored.packages}}{
#'
#'   A vector of packages, which should be ignored when attempting to snapshot
#'   the project's private library. Note that if a package has already been
#'   added to the lockfile, that entry in the lockfile will not be ignored. \cr
#'
#' }
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
#' }
#'
#' @section Defaults:
#'
#' You can change the default values of these settings for newly-created `renv`
#' projects by setting \R options for `renv.settings` or `renv.settings.<name>`.
#' For example:
#'
#' \preformatted{
#' options(renv.settings = list(snapshot.type = "simple"))
#' options(renv.settings.snapshot.type = "simple")
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

  snapshot.type = renv_settings_impl(
    name     = "snapshot.type",
    validate = function(x) x %in% c("simple", "packrat"),
    default  = "packrat",
    update   = NULL
  ),

  use.cache = renv_settings_impl(
    name     = "use.cache",
    validate = is.logical,
    default  = TRUE,
    update   = renv_settings_updated_cache
  )

)
