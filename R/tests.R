
renv_tests_scope <- function(packages = character()) {

  renv_tests_init()

  # ensure that attempts to restart are a no-op
  options(restart = function(...) TRUE)

  # save local repositories
  Sys.setenv(RENV_PATHS_LOCAL = file.path(renv_tests_root(), "local"))

  # move to own test directory
  dir <- tempfile("renv-test-")
  ensure_directory(dir)
  dir <- renv_path_normalize(dir, winslash = "/")
  owd <- setwd(dir)

  # set as active project
  Sys.setenv(RENV_PROJECT = dir)

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
  renv_global("tests.root", renv_tests_root_impl(path))
}

renv_tests_root_impl <- function(path = getwd()) {

  # construct set of paths we'll hunt through
  slashes <- gregexpr("(?:/|$)", path)[[1]]
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

renv_tests_init_envvars <- function() {
  root <- tempfile("renv-root-")
  dir.create(root, showWarnings = TRUE, mode = "755")
  Sys.setenv(RENV_PATHS_ROOT = root)
}

renv_tests_init_options <- function() {
  options(
    renv.config.user.library = FALSE,
    restart = NULL,
    warn = 2
  )
}

renv_tests_init_repos <- function(repos = NULL) {

  # find root directory
  root <- renv_tests_root()

  # generate our dummy repository
  repos <- repos %||% tempfile("renv-repos-")
  contrib <- file.path(repos, "src/contrib")
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
    desc$Version <- "0.1.0"
    write.dcf(desc, file = descpath)

    # place these packages into the archive
    upload(path, file.path(contrib, "Archive"), subdir = TRUE)

  }

  # update PACKAGES metadata
  tools::write_PACKAGES(contrib, subdirs = subdirs, type = "source")

  # set repository URL (for tests)
  options(renv.tests.repos = c(CRAN = repos))

  # and update our repos option
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  options(
    pkgType = "source",
    repos = c(CRAN = sprintf(fmt, repos))
  )

}

renv_tests_init_packages <- function() {

  # eagerly load packages that we'll need during tests
  # (as the sandbox will otherwise 'hide' these packages)
  packages <- c(
    "packrat",
    "knitr",
    "rappdirs",
    "reticulate",
    "rmarkdown",
    "uuid",
    "yaml"
  )

  for (package in packages)
    requireNamespace(package, quietly = TRUE)

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
  renv_sandbox_activate()

}

renv_tests_init_finish <- function() {
  options(renv.testing = TRUE)
}

renv_tests_init <- function() {

  if (renv_testing())
    return()

  Sys.unsetenv("RENV_PATHS_LIBRARY")
  Sys.unsetenv("RENV_PATHS_LIBRARY_ROOT")

  Sys.unsetenv("RENV_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON_ENV")

  renv_tests_init_workarounds()
  renv_tests_init_working_dir()
  renv_tests_init_envvars()
  renv_tests_init_options()
  renv_tests_init_repos()
  renv_tests_init_packages()
  renv_tests_init_sandbox()
  renv_tests_init_finish()

}

renv_tests_report <- function() {
  Sys.getenv()
}

renv_testing <- function() {
  getOption("renv.testing", default = FALSE)
}

renv_test_code <- function(code, fileext = ".R") {

  file <- tempfile("renv-code-", fileext = fileext)
  writeLines(deparse(substitute(code)), con = file)
  file

}

renv_test_retrieve <- function(record) {

  renv_scope_error_handler()

  # construct records
  package <- record$Package
  records <- list(record)
  names(records) <- package

  # prepare dummy library
  templib <- renv_tempfile("renv-library-")
  ensure_directory(templib)
  renv_scope_libpaths(c(templib, .libPaths()))

  # attempt a restore into that library
  renv_scope_restore(
    project = getwd(),
    records = records,
    packages = package,
    recursive = FALSE
  )

  records <- renv_retrieve(record$Package)
  library <- renv_libpaths_all()
  renv_install(records, library)

  descpath <- file.path(templib, package)
  if (!file.exists(descpath))
    stopf("failed to retrieve package '%s'", package)

  desc <- renv_description_read(descpath)
  fields <- grep("^Remote", names(record), value = TRUE)
  testthat::expect_identical(as.list(desc[fields]), as.list(record[fields]))

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

  # check packages in repository
  db <- as.data.frame(
    available.packages(type = "source"),
    stringsAsFactors = FALSE
  )

  # avoid printing too many
  n <- 10L
  packages <- head(db$Package, n = n)
  if (length(db$Package) > n) {
    rest <- paste("and", length(db$Package) - n, "more")
    packages <- c(packages, rest)
  }

  renv_pretty_print(
    packages,
    "The following packages are available in the test repositories:",
  )

  envvars <- c("R_LIBS", "R_LIBS_SITE", "R_LIBS_USER")
  keys <- format(envvars)
  vals <- Sys.getenv(keys, unset = "<NA>")
  vals[vals != "<NA>"] <- shQuote(vals[vals != "<NA>"], type = "cmd")
  renv_pretty_print(paste(keys, vals, sep = " : "), wrap = FALSE)

}
