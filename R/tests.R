
renv_tests_scope <- function(packages) {

  renv_tests_init()

  # move to own test directory
  dir <- tempfile("renv-test-")
  ensure_directory(dir)
  owd <- setwd(dir)

  # create file with dependencies
  code <- sprintf("library(%s)", packages)
  writeLines(code, "dependencies.R")

  # clean up when finished in parent scope
  defer({setwd(owd); renv_state_clear()}, envir = parent.frame())

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

  # move to packages directory
  owd <- setwd(file.path(root, "packages"))
  on.exit(setwd(owd), add = TRUE)

  # generate our dummy repository
  repos <- tempfile("renv-repos-")
  contrib <- file.path(repos, "src/contrib")
  ensure_directory(contrib)

  # copy in packages
  packages <- list.files()
  for (package in packages) {

    # create package tarball
    desc <- renv_description_read(package)
    tarball <- sprintf("%s_%s.tar.gz", package, desc$Version)
    tar(tarball, package, compression = "gzip", tar = "internal")

    # copy into repository tree
    target <- file.path(contrib, package, tarball)
    ensure_parent_directory(target)
    file.rename(tarball, target)

  }

  # update PACKAGES metadata
  tools::write_PACKAGES(contrib, subdirs = TRUE)

  # and update our repos option
  options(
    pkgType = "source",
    repos = c(CRAN = sprintf("file://%s", repos))
  )

}

renv_tests_init_packages <- function() {

  fields <- c("Depends", "Imports", "Suggests")
  dependencies <- renv_dependencies("renv", fields = fields)
  for (dependency in names(dependencies))
    requireNamespace(dependency, quietly = TRUE)

}

renv_tests_init <- function() {

  if (identical(Sys.getenv("RENV_TESTS_INITIALIZED"), "TRUE"))
    return()

  renv_tests_init_envvars()
  renv_tests_init_options()
  renv_tests_init_repos()
  renv_tests_init_packages()

}
