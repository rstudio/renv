
# Code that needs to run once, before a suite of tests is run.
# Here, "suite of tests" might also mean "a single test" interactively.
renv_tests_setup <- function(envir = parent.frame()) {

  # ensure that attempts to restart are a no-op
  if (renv_rstudio_available())
    options(restart = function(...) NULL)

  # cache path before working directory gets changed
  renv_tests_root()

  # make sure required packages are loaded
  # (not scoped to the environment since packages can't reliably be unloaded)
  renv_tests_setup_packages()

  # initialize test repositories
  renv_tests_setup_repos(envir = envir)

  # scope relevant environment variables
  renv_tests_setup_envvars(envir = envir)
  renv_tests_setup_options(envir = envir)

  # set up interactive tools
  renv_tests_setup_tools(envir)

}


renv_tests_setup_envvars <- function(envir = parent.frame()) {

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

  envvars <- Sys.getenv()
  configvars <- grep("^RENV_CONFIG_", names(envvars), value = TRUE)
  renv_scope_envvars(
    list = rep_named(configvars, list(NULL)),
    envir = envir
  )
}

renv_tests_setup_options <- function(envir = parent.frame()) {

  renv_scope_options(
    renv.bootstrap.quiet = TRUE,
    renv.config.user.library = FALSE,
    renv.config.sandbox.enabled = TRUE,
    restart = NULL,
    renv.config.install.transactional = FALSE,
    renv.tests.running = TRUE,
    envir = envir
  )

}

renv_tests_setup_tools <- function(envir) {

  if (interactive()) {

    # create a 'done' object that, when printed, will
    # run any pending defer handlers
    envir <- attach(NULL, name = "renv:tools")
    envir$done <- structure(list(), class = "renv_done")
    registerS3method("print", "renv_done", function(x, ...) {
      renv:::renv_defer_execute(globalenv())
    })

    # detach when we're done
    defer(detach("renv:tools"), envir = envir)

  }

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

renv_tests_setup_repos <- function(envir = parent.frame()) {

  # generate our dummy repository
  repopath <- file.path(tempdir(), "repos")
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

  # clean up when we're done
  defer(unlink(repopath, recursive = TRUE), envir = envir)

  # return path to on-disk repository
  repopath

}
