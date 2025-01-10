
# Code that needs to run once, before a suite of tests is run.
# Here, "suite of tests" might also mean "a single test" interactively.
renv_tests_setup <- function(scope = parent.frame()) {

  # only run if interactive, or if testing
  ok <- interactive() || testthat::is_testing()
  if (!ok)
    return()

  # make sure this only runs once
  if (!once())
    return()

  # force gitcreds to initialize early
  renv_download_auth_github(url = "https://github.com")

  # remove automatic tasks so we can capture explicitly in tests
  renv_task_unload()

  # cache path before working directory gets changed
  renv_tests_root()

  # make sure required packages are loaded
  # (not scoped to the environment since packages can't reliably be unloaded)
  renv_tests_setup_packages()

  # fix up the library paths if needed for testing
  renv_tests_setup_libpaths(scope = scope)

  # make sure we clean up sandbox on exit
  renv_tests_setup_sandbox(scope = scope)

  # initialize test repositories
  renv_tests_setup_repos(scope = scope)

  # scope relevant environment variables
  renv_tests_setup_envvars(scope = scope)
  renv_tests_setup_options(scope = scope)

}


renv_tests_setup_envvars <- function(scope = parent.frame()) {

  # set up root directory
  root <- renv_scope_tempfile("renv-root-", scope = scope)
  ensure_directory(root)

  # set up sandbox directory
  sandbox <- file.path(root, "sandbox")
  ensure_directory(sandbox)

  renv_scope_envvars(
    RENV_AUTOLOAD_ENABLED = FALSE,
    RENV_CONFIG_LOCKING_ENABLED = FALSE,
    RENV_DOWNLOAD_METHOD = NULL,
    RENV_PATHS_ROOT = root,
    RENV_PATHS_LIBRARY = NULL,
    RENV_PATHS_LIBRARY_ROOT = NULL,
    RENV_PATHS_LOCAL = NULL,
    RENV_PATHS_LOCKFILE = NULL,
    RENV_PATHS_RENV = NULL,
    RENV_PATHS_SANDBOX = sandbox,
    RENV_WATCHDOG_ENABLED = FALSE,
    RENV_WATCHDOG_DEBUG = FALSE,
    scope = scope
  )

  envvars <- Sys.getenv()
  configvars <- grep("^RENV_CONFIG_", names(envvars), value = TRUE)
  renv_scope_envvars(
    list = rep_named(configvars, list(NULL)),
    scope = scope
  )
}

renv_tests_setup_options <- function(scope = parent.frame()) {

  renv_scope_options(
    renv.bootstrap.quiet = TRUE,
    renv.config.user.library = FALSE,
    renv.config.sandbox.enabled = TRUE,
    renv.consent = TRUE,
    restart = NULL,
    renv.config.install.transactional = FALSE,
    renv.tests.running = TRUE,
    scope = scope
  )

}

# Force loading of packages from current .libPaths(); needed for packages
# that would otherwise loaded in a renv_tests_scope()
renv_tests_setup_packages <- function() {

  # load recursive dependencies of testthat
  deps <- renv_package_dependencies("testthat")
  for (dep in names(deps))
    requireNamespace(dep, quietly = TRUE)

  # also load remotes
  requireNamespace("remotes", quietly = TRUE)

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
    #
    # TODO(Kevin): This fails for me with:
    #
    # Error in `source_file()`:
    # ! In path: "/Users/kevin/r/pkg/renv/tests/testthat/helper-zzz.R"
    # Caused by error in `pak$remote()`:
    #   ! Subprocess is busy or cannot start
    tryCatch({
      pak <- renv_namespace_load("pak")
      pak$remote(function() {})
    }, error = function(e) {
      options(renv.pak.enabled = FALSE)
    })

  }

}

renv_tests_setup_libpaths <- function(scope = parent.frame()) {

  # remove the sandbox from the library paths, just in case we tried
  # to run tests from an R session where the sandbox was active
  old <- .libPaths()
  new <- grep("renv/sandbox", old, fixed = TRUE, invert = TRUE, value = TRUE)
  renv_scope_libpaths(new, scope = scope)

}

renv_tests_setup_sandbox <- function(scope = parent.frame()) {
  renv_scope_options(renv.sandbox.locking_enabled = FALSE)
  defer(renv_sandbox_unlock(), scope = scope)
}

renv_tests_setup_repos <- function(scope = parent.frame()) {

  # generate our dummy repository
  repopath <- renv_tests_repopath()
  if (file.exists(repopath))
    return()

  # create repository source directory
  contrib <- file.path(repopath, "src/contrib")
  ensure_directory(contrib)

  # copy package stuff to tempdir (because we'll mutate them a bit)
  source <- renv_tests_path("packages")
  target <- renv_scope_tempfile("renv-packages-", scope = scope)
  renv_file_copy(source, target)
  renv_scope_wd(target)

  # update the local packrat package version
  record <- renv_available_packages_latest(package = "packrat")
  dcf <- renv_dcf_read(file = "packrat/DESCRIPTION")
  dcf$Version <- record$Version
  renv_dcf_write(dcf, file = "packrat/DESCRIPTION")

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
    renv_file_move(tarball, target, overwrite = TRUE)

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
    desc$Depends <- gsub("99.99.99", "1.0.0", desc$Depends %||% "", fixed = TRUE)
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

  # return path to on-disk repository
  repopath

}

