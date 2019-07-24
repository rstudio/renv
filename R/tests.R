
renv_tests_scope <- function(packages = character()) {

  renv_tests_init()

  # ensure that attempts to restart are a no-op
  options(restart = function(...) TRUE)

  # move to own test directory
  dir <- tempfile("renv-test-")
  ensure_directory(dir)
  dir <- normalizePath(dir, winslash = "/")
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
  .libPaths(c(lib, .libPaths()))

  defer(envir = parent.frame(), {
    setwd(owd)
    unlink(lib, recursive = TRUE)
    .libPaths(libpaths)
  })

  invisible(dir)

}

renv_tests_root <- function(path = getwd()) {

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
  Sys.setenv(RENV_PATHS_ROOT = root, RENV_TESTS_INITIALIZED = "TRUE")
}

renv_tests_init_options <- function() {
  options(warn = 2, restart = NULL)
}

renv_tests_init_repos <- function() {

  # find root directory
  root <- renv_tests_root()

  # generate our dummy repository
  repos <- tempfile("renv-repos-")
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
  upload <- function(path, root) {

    # create package tarball
    desc <- renv_description_read(path)
    package <- basename(path)
    tarball <- sprintf("%s_%s.tar.gz", package, desc$Version)
    tar(tarball, package, compression = "gzip", tar = "internal")

    # copy into repository tree
    target <- file.path(root, package, tarball)
    ensure_parent_directory(target)
    file.rename(tarball, target)

  }

  # copy in packages
  paths <- list.files(getwd(), full.names = TRUE)
  for (path in paths) {

    # upload the 'regular' package
    upload(path, contrib)

    # generate an 'old' version of the packages
    descpath <- file.path(path, "DESCRIPTION")
    desc <- renv_description_read(descpath)
    desc$Version <- "0.1.0"
    write.dcf(desc, file = descpath)

    # place these packages into the archive
    upload(path, file.path(contrib, "Archive"))

  }

  # update PACKAGES metadata
  tools::write_PACKAGES(contrib, subdirs = TRUE, type = "source")

  # set repository URL (for tests)
  options(renv.tests.repos = c(CRAN = repos))

  # and update our repos option
  fmt <- if (renv_platform_windows()) "file:%s" else "file://%s"
  options(
    pkgType = "source",
    repos = c(CRAN = sprintf(fmt, repos))
  )

}

renv_tests_init_packages <- function() {

  fields <- c("Depends", "Imports", "Suggests")
  project <- Sys.getenv("RENV_PROJECT", unset = getwd())
  dependencies <- renv_dependencies(project, "renv", fields = fields)
  for (dependency in names(dependencies))
    requireNamespace(dependency, quietly = TRUE)

}

renv_tests_init_sandbox <- function() {
  renv_sandbox_activate()
}

renv_tests_init <- function() {

  # NOTE: we set both variables so we can distinguish between
  # R CMD check and devtools::test(). we do this dance just in
  # case RENV_TESTS_INITIALIZED gets set during interactive
  # debugging of renv tests
  if (renv_testing())
    return()

  renv_tests_init_workarounds()
  renv_tests_init_working_dir()
  renv_tests_init_envvars()
  renv_tests_init_options()
  renv_tests_init_repos()
  renv_tests_init_packages()
  renv_tests_init_sandbox()

}

renv_tests_report <- function() {
  Sys.getenv()
}

renv_testing <- function() {
  vars <- c("RENV_TESTS_INITIALIZED", "RENV_TESTS_INITIALIZED_TESTTHAT")
  vals <- Sys.getenv(vars, unset = NA)
  any(!is.na(vals))
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
  renv_restore_begin(records = records, packages = package, recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  records <- renv_retrieve(record$Package)
  library <- renv_libpaths_default()
  renv_install(records, library, getwd())

  desc <- renv_description_read(file.path(templib, package))

  fields <- grep("^Remote", names(record), value = TRUE)
  testthat::expect_identical(as.list(desc[fields]), as.list(record[fields]))

}
