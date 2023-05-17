renv_tests_scope <- function(packages = character(), project = NULL, envir = parent.frame()) {

  renv_tests_init()
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
  repopath <- global("test.repo.path", renv_tests_init_repos_impl())

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

renv_tests_init_repos_impl <- function() {
  # generate our dummy repository
  repopath <- tempfile("renv-tests-repos-")
  contrib <- file.path(repopath, "src/contrib")
  ensure_directory(contrib)

  # save current directory
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)

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
