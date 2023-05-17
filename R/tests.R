
renv_tests_program <- function(name) {
  program <- Sys.which(name)
  if (!nzchar(program))
    testthat::skip(paste("required program", name, "is not available"))
  program
}

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


renv_tests_init_envvars <- function() {

  Sys.unsetenv("RENV_PATHS_ROOT")
  Sys.unsetenv("RENV_PATHS_LIBRARY")
  Sys.unsetenv("RENV_PATHS_LIBRARY_ROOT")
  Sys.unsetenv("RENV_PATHS_LOCAL")
  Sys.unsetenv("RENV_PATHS_LOCKFILE")
  Sys.unsetenv("RENV_PATHS_RENV")

  Sys.setenv(RENV_AUTOLOAD_ENABLED = "FALSE")

  envvars <- Sys.getenv()
  configvars <- grep("^RENV_CONFIG_", names(envvars), value = TRUE)
  Sys.unsetenv(configvars)

}

renv_tests_init_workarounds <- function() {

  if (renv_platform_macos()) {

    if (!nzchar(Sys.getenv("TZ")))
      Sys.setenv(TZ = "America/Los_Angeles")

  }

}

renv_tests_init_options <- function() {

  # find path to renv sources
  sources <- renv_file_find(getwd(), function(parent) {
    descpath <- file.path(parent, "DESCRIPTION")
    if (file.exists(descpath))
      return(parent)
  })

  # set it so we can find the sources
  options(
    renv.test.sources = sources,
    renv.config.user.library = FALSE,
    renv.config.sandbox.enabled = TRUE,
    restart = NULL,
    warn = 2
  )

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

renv_tests_init_packages <- function() {
  # Force loading of packages from current .libPaths(); needed for packages
  # that would otherwise loaded in a renv_tests_scope()
  requireNamespace("waldo")
  renv_namespace_load("crayon")

  if (!isNamespaceLoaded("pak")) {
    usr <- file.path(tempdir(), "usr-cache")
    ensure_directory(file.path(usr, "R/renv"))

    pkg <- file.path(tempdir(), "pkg-cache")
    ensure_directory(pkg)

    renv_scope_envvars(
      R_USER_CACHE_DIR = usr,
      R_PKG_CACHE_DIR  = pkg
    )

    requireNamespace("pak")
    # trigger package load in pak subprocess
    pak <- renv_namespace_load("pak")
    pak$remote(function() {})
  }
}

renv_tests_init_finish <- function() {

  # remove any leftover renv-test- directories in the userdir
  userdir <- renv_bootstrap_user_dir()
  libdir <- file.path(userdir, "library")
  testdirs <- list.files(
    path = libdir,
    pattern = "^renv-test-",
    full.names = TRUE
  )
  unlink(testdirs, recursive = TRUE)

  # don't perform transactional installs by default for now
  # (causes strange CI failures, especially on Windows?)
  options(renv.config.install.transactional = FALSE)

  # mark tests as running
  options(renv.tests.running = TRUE)

}

renv_tests_init <- function() {

  if (renv_tests_running())
    return()

  renv_tests_init_envvars()

  # cache path before working directory gets changed
  renv_tests_root()

  renv_tests_init_envvars()
  renv_tests_init_workarounds()
  renv_tests_init_options()
  renv_tests_init_packages()
  renv_tests_init_finish()

}

renv_tests_running <- function() {
  getOption("renv.tests.running", default = FALSE)
}

renv_test_code <- function(code, data = list(), fileext = ".R") {
  code <- do.call(substitute, list(substitute(code), data))
  file <- tempfile("renv-code-", fileext = fileext)
  writeLines(deparse(code), con = file)
  file
}

renv_test_retrieve <- function(record) {

  renv_scope_error_handler()

  # avoid using cache
  renv_scope_envvars(RENV_PATHS_CACHE = tempfile())

  # construct records
  package <- record$Package
  records <- list(record)
  names(records) <- package

  # prepare dummy library
  templib <- renv_scope_tempfile("renv-library-")
  ensure_directory(templib)
  renv_scope_libpaths(c(templib, .libPaths()))

  # attempt a restore into that library
  renv_scope_restore(
    project = getwd(),
    library = templib,
    records = records,
    packages = package,
    recursive = FALSE
  )

  records <- retrieve(record$Package)
  renv_install_impl(records)

  descpath <- file.path(templib, package)
  if (!file.exists(descpath))
    stopf("failed to retrieve package '%s'", package)

  desc <- renv_description_read(descpath)
  fields <- grep("^Remote", names(record), value = TRUE)

  testthat::expect_identical(
    as.list(desc[fields]),
    as.list(record[fields])
  )

}

renv_tests_diagnostics <- function() {

  # print library paths
  renv_pretty_print(
    paste("-", .libPaths()),
    "The following R libraries are set:",
    wrap = FALSE
  )

  # print repositories
  repos <- getOption("repos")
  renv_pretty_print(
    paste(names(repos), repos, sep = ": "),
    "The following repositories are set:",
    wrap = FALSE
  )

  # print renv root
  renv_pretty_print(
    paste("-", paths$root()),
    "The following renv root directory is being used:",
    wrap = FALSE
  )

  # print cache root
  renv_pretty_print(
    paste("-", paths$cache()),
    "The following renv cache directory is being used:",
    wrap = FALSE
  )

  writeLines("The following packages are available in the test repositories:")

  dbs <-
    available_packages(type = "source", quiet = TRUE) %>%
    map(function(db) {
      rownames(db) <- NULL
      db[c("Package", "Version", "File")]
    })

  print(dbs)

  path <- Sys.getenv("PATH")
  splat <- strsplit(path, .Platform$path.sep, fixed = TRUE)[[1]]

  renv_pretty_print(
    paste("-", splat),
    "The following PATH is set:",
    wrap = FALSE
  )

  envvars <- c(
    grep("^_R_", names(Sys.getenv()), value = TRUE),
    "HOME",
    "R_ARCH", "R_HOME",
    "R_LIBS", "R_LIBS_SITE", "R_LIBS_USER", "R_USER",
    "R_ZIPCMD",
    "TAR", "TEMP", "TMP", "TMPDIR"
  )

  keys <- format(envvars)
  vals <- Sys.getenv(envvars, unset = "<NA>")
  vals[vals != "<NA>"] <- renv_json_quote(vals[vals != "<NA>"])

  renv_pretty_print(
    paste(keys, vals, sep = " : "),
    "The following environment variables of interest are set:",
    wrap = FALSE
  )

}

# Cache absolute path to tests/testthat
renv_tests_root <- function() {
  global("tests.root", normalizePath(testthat::test_path(".")))
}

renv_tests_path <- function(path = NULL) {
  if (is.null(path)) {
    renv_tests_root()
  } else {
    file.path(renv_tests_root(), path)
  }
}

renv_tests_supported <- function() {

  # supported when running locally + on CI
  for (envvar in c("NOT_CRAN", "CI"))
    if (!is.na(Sys.getenv(envvar, unset = NA)))
      return(TRUE)

  # disabled on older macOS releases (credentials fails to load)
  if (renv_platform_macos() && getRversion() < "4.0.0")
    return(FALSE)

  # disabled on Windows
  if (renv_platform_windows())
    return(FALSE)

  # true otherwise
  TRUE

}
