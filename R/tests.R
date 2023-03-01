
renv_tests_program <- function(name) {
  program <- Sys.which(name)
  if (!nzchar(program))
    testthat::skip(paste("required program", name, "is not available"))
  program
}

renv_tests_scope <- function(packages = character(), project = NULL) {

  renv_tests_init()

  # ensure that attempts to restart are a no-op
  options(restart = function(...) TRUE)

  # save local repositories
  Sys.setenv(RENV_PATHS_LOCAL = file.path(renv_tests_root(), "local"))

  # move to own test directory
  dir <- project %||% tempfile("renv-test-")
  ensure_directory(dir)
  dir <- renv_path_normalize(dir, winslash = "/")
  owd <- setwd(dir)

  # set as active project
  Sys.setenv(RENV_PROJECT = dir)

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

  defer(envir = parent.frame(), {
    setwd(owd)
    unlink(lib, recursive = TRUE)
    .libPaths(libpaths)
  })

  invisible(dir)

}

renv_tests_root <- function(path = getwd()) {
  global("tests.root", renv_tests_root_impl(path))
}

renv_tests_root_impl <- function(path = getwd()) {

  # if we're working in an RStudio project, we can cheat
  if (exists(".rs.getProjectDirectory")) {
    projroot <- get(".rs.getProjectDirectory")
    return(file.path(projroot(), "tests/testthat"))
  }

  # construct set of paths we'll hunt through
  slashes <- gregexpr("(?:/|$)", path, perl = TRUE)[[1]]
  parts <- substring(path, 1, slashes - 1)

  # begin the search
  for (part in rev(parts)) {

    # required to find test directory during R CMD check
    if (file.exists(file.path(part, "testthat.R")))
      return(file.path(part, "testthat"))

    # required for other general testing
    anchor <- file.path(part, "DESCRIPTION")
    if (file.exists(anchor))
      return(file.path(part, "tests/testthat"))

  }

  stop("could not determine root directory for test files")

}

renv_tests_init_envvars <- function() {

  Sys.unsetenv("RENV_PROFILE")
  Sys.unsetenv("RENV_PROJECT")
  Sys.unsetenv("RENV_PATHS_ROOT")
  Sys.unsetenv("RENV_PATHS_LIBRARY")
  Sys.unsetenv("RENV_PATHS_LIBRARY_ROOT")
  Sys.unsetenv("RENV_PATHS_LOCAL")
  Sys.unsetenv("RENV_PATHS_LOCKFILE")
  Sys.unsetenv("RENV_PATHS_RENV")

  Sys.unsetenv("RENV_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON_ENV")
  Sys.unsetenv("RETICULATE_PYTHON_FALLBACK")

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

renv_tests_init_working_dir <- function() {
  if (exists(".rs.getProjectDirectory")) {
    home <- get(".rs.getProjectDirectory")
    setwd(home())
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

renv_tests_init_repos <- function(repopath = NULL) {

  # find root directory
  root <- renv_tests_root()

  # generate our dummy repository
  repopath <- repopath %||% tempfile("renv-tests-repos-")
  contrib <- file.path(repopath, "src/contrib")
  ensure_directory(contrib)

  # save current directory
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)

  # copy package stuff to tempdir (because we'll mutate them a bit)
  source <- file.path(root, "packages")
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

  # update our repos option
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  repos <- c(CRAN = sprintf(fmt, repopath))

  options(
    pkgType             = "source",
    repos               = repos,
    renv.tests.repos    = repos,
    renv.tests.repopath = repopath
  )

}

renv_tests_init_packages <- function() {

  # don't treat warnings as errors in this scope
  renv_scope_options(warn = 1)

  # find packages to load
  packages <- renv_tests_init_packages_find()

  # load those packages
  envir <- new.env(parent = emptyenv())
  renv_tests_init_packages_load(packages, envir)

}

renv_tests_init_packages_find <- function() {
  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")
  descpath <- system.file("DESCRIPTION", package = "renv")
  deps <- renv_dependencies_discover_description(descpath, fields)
  deps[["Package"]]
}

renv_tests_init_packages_load <- function(packages, envir) {
  for (package in packages) {
    tryCatch(
      renv_tests_init_packages_load_impl(package, envir),
      error = warning
    )
  }
}

renv_tests_init_packages_load_impl <- function(package, envir) {

  # skip the 'R' package
  if (identical(package, "R"))
    return()

  # if we've already tried to load this package, skip it
  if (visited(package, envir = envir))
    return()

  # try to load the package
  if (!package %in% loadedNamespaces()) {
    if (!requireNamespace(package, quietly = TRUE)) {
      fmt <- "Failed to load package '%s' (required for testing)"
      writef(fmt, package)
      return()
    }
  }

  # try to find this package
  pkgpath <- renv_package_find(package)
  if (!file.exists(pkgpath))
    return()

  # try to read the package DESCRIPTION and load its dependencies
  descpath <- file.path(pkgpath, "DESCRIPTION")
  deps <- renv_dependencies_discover_description(
    path   = descpath,
    fields = c("Depends", "Imports", "LinkingTo")
  )

  map(
    deps$Package,
    renv_tests_init_packages_load,
    envir = envir
  )

}

renv_tests_init_sandbox <- function() {

  # eagerly load packages that we'll need during tests
  # (as the sandbox will otherwise 'hide' these packages)
  testthat <- find.package("testthat")
  descpath <- file.path(testthat, "DESCRIPTION")
  deps <- renv_dependencies_discover_description(descpath)
  for (package in deps$Package)
    requireNamespace(package, quietly = TRUE)

  # set up a dummy library path
  dummy <- tempfile("renv-library-")
  dir.create(dummy)
  .libPaths(dummy)

  # now sandbox the libpaths
  Sys.setenv(RENV_PATHS_SANDBOX = tempdir())
  renv_sandbox_activate()

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

  # make sure the sandbox is writable on shutdown so R can clean it up
  reg.finalizer(renv_envir_self(), function(object) {
    sandbox <- renv_sandbox_path()
    if (file.exists(sandbox))
      Sys.chmod(sandbox, "0755")
  }, onexit = TRUE)

}

renv_tests_init <- function() {

  if (renv_tests_running())
    return()

  renv_tests_init_envvars()
  renv_tests_init_workarounds()
  renv_tests_init_working_dir()
  renv_tests_init_options()
  renv_tests_init_repos()
  renv_tests_init_packages()
  renv_tests_init_sandbox()
  renv_tests_init_finish()

}

renv_tests_running <- function() {
  getOption("renv.tests.running", default = FALSE)
}

renv_tests_verbose <- function() {

  # if we're not running tests, mark as true
  if (!renv_tests_running())
    return(TRUE)

  # otherwise, respect option
  # (we might set this to FALSE to silence output from expected errors)
  getOption("renv.tests.verbose", default = TRUE)

}

renv_test_code <- function(code, fileext = ".R") {

  file <- tempfile("renv-code-", fileext = fileext)
  writeLines(deparse(substitute(code)), con = file)
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
      db[c("Package", "Version", "Path")]
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

renv_tests_report <- function(test, elapsed, expectations) {

  # figure out overall test result
  status <- "PASS"
  for (expectation in expectations) {

    errors <- c("expectation_error", "expectation_failure")
    if (inherits(expectation, errors)) {
      status <- "FAIL"
      break
    }

    if (inherits(expectation, "expectation_skip")) {
      status <- "SKIP"
      break
    }

  }

  # get console width
  width <- max(getOption("width"), 78L)

  # write out text with line
  left <- trunc(test, width - 23L)

  # figure out how long tests took to run
  time <- if (elapsed < 0.1)
    "<0.1s"
  else
    format(renv_difftime_format_short(elapsed), width = 5L, justify = "right")

  # write formatted
  fmt <- "[%s / %s]"
  right <- sprintf(fmt, status, time)

  # fill space between with dots
  dots <- rep.int(".", max(0L, width - nchar(left) - nchar(right) - 4L))
  all <- paste(left, paste(dots, collapse = ""), right)

  # write it out
  cli::cat_bullet(all)

}

renv_tests_path <- function(path) {
  root <- renv_tests_root()
  file.path(root, path)
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
