
renv_tests_scope <- function(packages = character(),
                             project = NULL,
                             scope = parent.frame())
{
  # use private repositories in this scope
  renv_tests_scope_repos(scope = scope)

  # use sandbox in this scope, to guard against packages like 'bread'
  # being installed into the system library
  renv_scope_sandbox(scope = scope)

  # most tests will call init() which changes `R_LIBS_USER`;
  # this ensures we reset to the original value when the test is done
  renv_scope_envvars(R_LIBS_USER = NULL, scope = scope)

  # also ensure we 'exit' the current project after completion
  renv_scope_envvars(RENV_PROJECT = "", scope = scope)

  # move to own test directory
  project <- project %||% renv_scope_tempfile("renv-test-", scope = scope)
  ensure_directory(project)
  renv_scope_wd(project, scope = scope)

  # scope project here so that it's unset after load, just in case
  defer(renv_project_clear(), scope = scope)

  # create empty renv directory
  dir.create("renv")

  # create file with dependencies
  code <- sprintf("library(%s)", packages)
  writeLines(code, "dependencies.R")

  # use temporary library
  library <- renv_scope_tempfile("renv-library-", scope = scope)
  ensure_directory(library)
  renv_scope_libpaths(library, scope = scope)

  # return path to project directory
  invisible(project)
}

renv_tests_scope_repos <- function(scope = parent.frame()) {

  # get path to on-disk repository
  repopath <- renv_tests_repopath()

  # update our repos option
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  repos <- c(CRAN = sprintf(fmt, repopath))

  renv_scope_options(
    pkgType = "source",
    repos   = repos,
    scope   = scope
  )

}

renv_tests_scope_system_cache <- function(scope = parent.frame()) {
  skip_on_cran()
  renv_scope_options(repos = c(CRAN = "https://cloud.r-project.org"), scope = scope)
  renv_scope_envvars(RENV_PATHS_ROOT = renv_paths_root_default_impl(), scope = scope)
}

renv_scope_local <- function() {
  path <- renv_tests_path("local")
  renv_scope_envvars(RENV_PATHS_LOCAL = path, scope = parent.frame())
}
