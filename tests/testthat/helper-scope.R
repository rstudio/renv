
renv_tests_scope <- function(packages = character(),
                             project = NULL,
                             envir = parent.frame())
{
  # use private repositories in this scope
  renv_tests_scope_repos(envir = envir)

  # use sandbox in this scope
  # TODO(Kevin): Without this, tests fail trying to install BiocManager?
  renv_scope_sandbox(envir = envir)

  # most tests will call init() which changes `R_LIBS_USER`;
  # this ensures we reset to the original value when the test is done
  renv_scope_envvars(R_LIBS_USER = NULL, envir = envir)

  # also ensure we 'exit' the current project after completion
  renv_scope_envvars(RENV_PROJECT = "", envir = envir)

  # move to own test directory
  dir <- project %||% tempfile("renv-test-")
  ensure_directory(dir)
  renv_scope_setwd(dir, envir = envir)

  # create empty renv directory
  dir.create(file.path(dir, "renv"))

  # create file with dependencies
  code <- sprintf("library(%s)", packages)
  writeLines(code, "dependencies.R")

  # use temporary library
  lib <- tempfile("renv-library-")
  ensure_directory(lib)
  renv_scope_libpaths(lib, envir = envir)

  # make sure we clean up the library path when we're done too
  defer(unlink(lib, recursive = TRUE), envir = envir)

  # return path to project directory
  invisible(dir)
}

renv_tests_scope_repos <- function(envir = parent.frame()) {

  repopath <- file.path(tempdir(), "repos")

  # update our repos option
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  repos <- c(CRAN = sprintf(fmt, repopath))

  renv_scope_options(
    pkgType             = "source",
    repos               = repos,
    renv.tests.repos    = repos,
    renv.tests.repopath = repopath,
    envir = envir
  )

}
