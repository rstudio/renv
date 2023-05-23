
renv_tests_scope <- function(packages = character(),
                             project = NULL,
                             envir = parent.frame())
{
  # source setup.R if necessary (for interactive scenarios)
  running <- getOption("renv.tests.running", default = FALSE)
  if (!running) {
    path <- test_path("setup.R")
    sys.source(path, envir = globalenv())
  }

  # use local repositories in this scope
  renv_tests_scope_repos(envir = envir)

  # most tests will call init() which changes `R_LIBS_USER`;
  # this ensures we reset to the original value when the test is done
  renv_scope_envvars(R_LIBS_USER = NULL, envir = envir)

  # ensure that attempts to restart are a no-op
  if (renv_rstudio_available())
    options(restart = function(...) TRUE)

  # move to own test directory
  dir <- project %||% tempfile("renv-test-")
  ensure_directory(dir)
  dir <- renv_path_normalize(dir, winslash = "/")
  owd <- setwd(dir)

  # create empty renv directory
  dir.create(file.path(dir, "renv"))

  # create file with dependencies
  code <- sprintf("library(%s)", packages)
  writeLines(code, "dependencies.R")

  # use temporary library
  lib <- tempfile("renv-library-")
  ensure_directory(lib)
  libpaths <- .libPaths()
  .libPaths(lib)

  defer(envir = envir, {
    setwd(owd)
    unlink(lib, recursive = TRUE)
    .libPaths(libpaths)
  })

  invisible(dir)
}

renv_tests_scope_repos <- function(envir = parent.frame()) {
  repopath <- global("test.repo.path", renv_tests_repos_impl())

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

renv_tests_repos_impl <- function() {
  # generate our dummy repository
  repopath <- tempfile("renv-tests-repos-")
  contrib <- file.path(repopath, "src/contrib")
  ensure_directory(contrib)

  # save current directory
  owd <- getwd()
  defer(setwd(owd))

  # copy package stuff to tempdir (because we'll mutate them a bit)
  source <- renv_tests_path("packages")
  target <- tempfile("renv-packages-")
  renv_file_copy(source, target)
  setwd(target)

  # helper function for 'uploading' a package to our test repo
  upload <- function(path, root, subdir = FALSE) {

    # create package tarball
    desc <- renv_description_read(path)
    package <- basename(path)
    tarball <- sprintf("%s_%s.tar.gz", package, desc$Version)
    tar(tarball, package, compression = "gzip")

    # copy into repository tree
    components <- c(root, if (subdir) package, tarball)
    target <- paste(components, collapse = "/")
    ensure_parent_directory(target)
    renv_file_move(tarball, target)

  }

  # just in case?
  renv_scope_options(renv.config.filebacked.cache = FALSE)

  # copy in packages
  paths <- list.files(getwd(), full.names = TRUE)
  subdirs <- file.path(getRversion(), "Recommended")
  for (path in paths) {

    # upload the 'regular' package
    upload(path, contrib, subdir = FALSE)

    # upload a subdir (mocking what R does during upgrades)
    upload(path, file.path(contrib, subdirs), subdir = FALSE)

    # generate an 'old' version of the packages
    descpath <- file.path(path, "DESCRIPTION")
    desc <- renv_description_read(descpath)
    desc$Version <- "0.0.1"
    write.dcf(desc, file = descpath)

    # place packages at top level (simulating packages with multiple
    # versions at the top level of the repository)
    upload(path, contrib, subdir = FALSE)

    # generate an 'old' version of the packages
    descpath <- file.path(path, "DESCRIPTION")
    desc <- renv_description_read(descpath)
    desc$Version <- "0.1.0"
    write.dcf(desc, file = descpath)

    # place these packages into the archive
    upload(path, file.path(contrib, "Archive"), subdir = TRUE)

  }

  # update PACKAGES metadata
  tools::write_PACKAGES(
    dir = contrib,
    subdirs = subdirs,
    type = "source",
    latestOnly = FALSE
  )

  repopath
}


# overall infrastructure --------------------------------------------------

# This function is designed to be run once before all tests are run in
# setup.R. It's also provided here in case you need to step through a
# test line by line.
renv_tests_scope_setup <- function(envir = parent.frame()) {

  # cache path before working directory gets changed
  renv_tests_root()

  # scope relevant environment variables
  renv_tests_scope_envvars(envir = envir)
  renv_tests_scope_options(envir = envir)

  # make sure required packages are loaded
  renv_tests_init_packages()

}

renv_tests_scope_envvars <- function(envir = parent.frame()) {

  # set up root directory
  root <- normalizePath(
    ensure_directory(renv_scope_tempfile(envir = envir)),
    mustWork = FALSE,
    winslash = "/"
  )

  renv_scope_envvars(
    # simulate running in R CMD check
    "_R_CHECK_PACKAGE_NAME_" = "renv",
    # disable locking in this scope
    RENV_CONFIG_LOCKING_ENABLED = FALSE,
    RENV_PATHS_ROOT = root,
    RENV_PATHS_LIBRARY = NULL,
    RENV_PATHS_LIBRARY_ROOT = NULL,
    RENV_PATHS_LOCAL = NULL,
    RENV_PATHS_LOCKFILE = NULL,
    RENV_PATHS_RENV = NULL,
    RENV_AUTOLOAD_ENABLED = FALSE,
    envir = envir
  )

  if (is.na(Sys.getenv("GITHUB_PATH", unset = NA))) {
    token <- tryCatch(gitcreds::gitcreds_get(), error = function(e) NULL)
    if (!is.null(token)) {
      renv_scope_envvars(GITHUB_PAT = token$password, envir = envir)
    }
  }

  needs_tz <- renv_platform_macos() && !nzchar(Sys.getenv("TZ"))
  if (needs_tz) {
    renv_scope_envvars(
      TZ = "America/Los_Angeles",
      envir = envir
    )
  }

  envvars <- Sys.getenv()
  configvars <- grep("^RENV_CONFIG_", names(envvars), value = TRUE)
  renv_scope_envvars(
    list = rep_named(configvars, list(NULL)),
    envir = envir
  )
}

renv_tests_scope_options <- function(envir = parent.frame()) {

  # find path to renv sources
  sources <- renv_file_find(getwd(), function(parent) {
    descpath <- file.path(parent, "DESCRIPTION")
    if (file.exists(descpath))
      return(parent)
  })

  renv_scope_options(
    renv.bootstrap.quiet = TRUE,
    # set it so we can find the sources
    renv.test.sources = sources,
    renv.config.user.library = FALSE,
    renv.config.sandbox.enabled = TRUE,
    restart = NULL,
    # don't perform transactional installs by default for now
    # (causes strange CI failures, especially on Windows?)
    renv.config.install.transactional = FALSE,
    # mark tests as running
    renv.tests.running = TRUE,
    envir = envir

  )
}

# Force loading of packages from current .libPaths(); needed for packages
# that would otherwise loaded in a renv_tests_scope()
renv_tests_init_packages <- function() {

  # All recursive testthat deps
  pkgs <- global("testthat_deps", renv_tests_testthat_dependencies_impl())
  for (pkg in pkgs) {
    renv_namespace_load(pkg)
  }

  # Also load remotes
  renv_namespace_load("remotes")

  # pak needs a little special handling
  if (renv_package_installed("pak")) {

    # set environment variables that influence pak
    usr <- file.path(tempdir(), "usr-cache")
    ensure_directory(file.path(usr, "R/renv"))

    pkg <- file.path(tempdir(), "pkg-cache")
    ensure_directory(pkg)

    renv_scope_envvars(
      R_USER_CACHE_DIR = usr,
      R_PKG_CACHE_DIR  = pkg
    )

    # load pak now
    requireNamespace("pak", quietly = TRUE)

    # trigger package load in pak subprocess
    pak <- renv_namespace_load("pak")
    pak$remote(function() {})

  }

}

renv_tests_testthat_dependencies_impl <- function() {
  db <- available_packages(type = "source")[[1]]
  sort(tools::package_dependencies("testthat", db, recursive = TRUE)[[1]])
}

