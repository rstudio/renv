
renv_tests_scope <- function(packages = character(),
                             project = NULL,
                             envir = parent.frame())
{
  # use private repositories in this scope
  renv_tests_scope_repos(envir = envir)

  # use sandbox in this scope, to guard against packages like 'bread'
  # being installed into the system library
  renv_scope_sandbox(envir = envir)

  # most tests will call init() which changes `R_LIBS_USER`;
  # this ensures we reset to the original value when the test is done
  renv_scope_envvars(R_LIBS_USER = NULL, envir = envir)

  # also ensure we 'exit' the current project after completion
  renv_scope_envvars(RENV_PROJECT = "", envir = envir)

  # move to own test directory
  project <- project %||% renv_scope_tempfile("renv-test-", envir = envir)
  ensure_directory(project)
  renv_scope_wd(project, envir = envir)

  # scope project here so that it's unset after load, just in case
  defer(renv_project_clear(), envir = envir)

  # create empty renv directory
  dir.create("renv")

  # create file with dependencies
  code <- sprintf("library(%s)", packages)
  writeLines(code, "dependencies.R")

  # use temporary library
  lib <- renv_scope_tempfile("renv-library-", envir = envir)
  ensure_directory(lib)
  renv_scope_libpaths(lib, envir = envir)

  # return path to project directory
  invisible(project)
}

renv_tests_scope_repos <- function(envir = parent.frame()) {

  # get path to on-disk repository
  repopath <- renv_tests_repopath()

  # update our repos option
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  repos <- c(CRAN = sprintf(fmt, repopath))

  renv_scope_options(
    pkgType = "source",
    repos   = repos,
    envir   = envir
  )

}

renv_tests_scope_system_cache <- function(envir = parent.frame()) {
  skip_on_cran()
  renv_scope_options(repos = c(CRAN = "https://cloud.r-project.org"), envir = envir)
  renv_scope_envvars(RENV_PATHS_ROOT = renv_paths_root_default_impl(), envir = envir)
}

renv_scope_local <- function() {
  path <- renv_tests_path("local")
  renv_scope_envvars(RENV_PATHS_LOCAL = path, envir = parent.frame())
}
