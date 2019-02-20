
# global variables corresponding to some project-specific state
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
  `_renv_state`[[name]] %||% invoke(default)
}

renv_state_impl_set <- function(name, value, default) {
  state <- renv_state_impl_get(name, default)
  `_renv_state`[[name]] <- value
  state
}

renv_state_impl_scoped <- function(name, value, default, envir) {
  state <- renv_state_impl_set(name, value, default)
  defer(renv_state_impl_set(name, state, default), envir = envir)
  state
}

renv_state_clear <- function() {
  rm(list = ls(`_renv_state`), envir = `_renv_state`)
}

renv_state <- list(
  project     = renv_state_impl("project",     function() getwd()),
  python      = renv_state_impl("python",      NULL)
)

renv_active_lockfile <- function(project = NULL) {
  project <- project %||% renv_state$project()
  path <- file.path(project, "renv/lockfile")
  lockfiles <- list.files(path, full.names = TRUE, recursive = TRUE)
  if (empty(lockfiles)) "" else tail(sort(lockfiles), n = 1L)
}
