
# A container for global variables that correspond to some project-specific
# state.
`_renv_state` <- new.env(parent = emptyenv())

renv_state_impl <- function(name, default) {

  function(value, scoped = FALSE) {
    if (missing(value))
      renv_state_impl_get(name, default)
    else if (!scoped)
      renv_state_impl_set(name, value, default)
    else
      renv_state_impl_scoped(name, value, default, parent.frame())
  }

}

renv_state_impl_get <- function(name, default) {
  if (exists(name, envir = `_renv_state`, inherits = FALSE))
    get(name, envir = `_renv_state`, inherits = FALSE)
  else
    invoke(default)
}

renv_state_impl_set <- function(name, value, default) {
  state <- renv_state_impl_get(name, default)
  assign(name, value, envir = `_renv_state`, inherits = FALSE)
  state
}

renv_state_impl_scoped <- function(name, value, default, envir) {
  state <- renv_state_impl_set(name, value, default)
  defer(renv_state_impl_set(name, state, default), envir = envir)
  state
}

renv_state <- list(
  project     = renv_state_impl("project",     function() getwd()),
  environment = renv_state_impl("environment", ""),
  local       = renv_state_impl("local",       FALSE)
)

renv_active_manifest <- function(project = NULL) {
  project <- project %||% renv_state$project()
  path <- file.path(project, "renv/manifest")
  manifests <- list.files(path, full.names = TRUE, recursive = TRUE)
  if (empty(manifests)) "" else tail(sort(manifests), n = 1L)
}
