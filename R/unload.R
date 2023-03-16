
unload <- function(project = NULL, quiet = FALSE) {

  project <- renv_project_resolve(project)
  renv_scope_error_handler()

  if (renv_tests_running())
    return()

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  renv_envvars_restore()

  renv_unload_shims(project)
  renv_unload_project(project)
  renv_unload_profile(project)
  renv_unload_envvars(project)
  renv_unload_sandbox(project)
  renv_unload_libpaths(project)
  renv_unload_profile(project)

}

renv_unload_shims <- function(project) {
  renv_shims_deactivate()
}

renv_unload_project <- function(project) {
  options(renv.project.path = NULL)
  Sys.unsetenv("RENV_PROJECT")
}

renv_unload_profile <- function(project) {
  Sys.unsetenv("RENV_PROFILE")
}

renv_unload_envvars <- function(project) {
  renv_envvars_restore()
}

renv_unload_sandbox <- function(project) {
  renv_sandbox_deactivate()
}

renv_unload_libpaths <- function(project) {
  renv_libpaths_restore()
}

renv_unload_profile <- function(project) {
  Sys.unsetenv("RENV_PROFILE")
}
