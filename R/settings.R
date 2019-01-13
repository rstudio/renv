
# TODO: project-local settings / options
renv_setting_definition_logical <- function(default) {
  list(
    decode   = as.logical,
    validate = is.logical,
    default  = default
  )
}

`_renv_setting_definitions` <- list(
  # TODO: what kinds of settings are appropriate? Some candidates:
  #
  # Use the global cache?
  # On restore, allow version mismatches? (always download latest)
  # Whitelist / blacklist for dependency discovery?

)

renv_settings_decode <- function(name, encoded) {
  decode <- `_renv_setting_definitions`[[name]]$decode
  catch(decode(encoded))
}

renv_settings_validate <- function(name, value) {
  validate <- `_renv_setting_definitions`[[name]]$validate
  catch(validate(value))
}

renv_settings_default <- function(name) {
  `_renv_setting_definitions`[[name]]$default
}

renv_settings_defaults <- function() {
  extract(`_renv_setting_definitions`, "default")
}

renv_settings_read <- function() {

  project <- renv_state$project()
  path <- file.path(project, "renv/renv.opts")
  if (!file.exists(path))
    return(renv_settings_defaults())

  dcf <- catch(renv_dcf_read(path))
  if (inherits(dcf, "error"))
    return(renv_settings_defaults())

  settings <- enumerate(dcf, function(name, encoded) {

    decoded <- catch(renv_settings_decode(name, encoded))
    if (inherits(decoded, "error"))
      return(renv_settings_default(name))

    if (!renv_settings_validate(name, decoded))
      return(renv_settings_default(name))

    decoded

  })

  renv_filebacked_set(path, settings)

}

renv_settings_get <- function(name) {
  project <- renv_state$project()

  path <- file.path(project, "renv/renv.opts")
  cache <- renv_filebacked_get(path)
  if (!is.null(cache))
    return(cache[[name]] %||% renv_settings_default(name))

  settings <- renv_settings_read()
  settings[[name]] %||% renv_settings_default(name)
}

renv_settings_set <- function(name, value, persist = TRUE) {

  project <- renv_state$project()
  path <- file.path(project, "renv/renv.opts")

  settings <- renv_filebacked_get(path) %||% renv_settings_read()
  settings[[name]] <- value
  renv_filebacked_set(path, settings)

  if (persist)
    renv_settings_persist(settings)
}

renv_settings_persist <- function(settings) {

  project <- renv_state$project()
  path <- file.path(project, "renv/renv.opts")

  # TODO: use explicit encoder here?
  lines <- paste(names(settings), settings, sep = ": ")
  writeLines(lines, con = path)

}

renv_settings_impl <- function(name) {

  force(name)
  function(value, persist = TRUE) {
    if (missing(value))
      renv_settings_get(name)
    else
      renv_settings_set(name, value, persist)
  }

}

#' Project-local Settings
#'
#' Define project-local settings, for fine-tuning the interaction between
#' your project and its associated virtual environment.
#'
settings <- list()
