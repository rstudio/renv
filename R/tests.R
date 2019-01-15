
renv_tests_scope <- function(packages) {

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

renv_tests_init_envvars <- function() {
  root <- tempfile("renv-root-")
  dir.create(root, showWarnings = TRUE, mode = "755")
  Sys.setenv(RENV_PATHS_ROOT = root)
}

renv_tests_init_repos <- function() {

  # move to packages directory
  owd <- setwd("packages")
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

  # require the namespaces of all used packages (we need to load 'cli' eagerly
  # as otherwise it won't be available later when we mutate the library paths)
  renv <- find.package("renv")
  desc <- renv_description_read(renv)
  suggests <- strsplit(desc$Suggests, "\\s*,\\s*")[[1]]
  for (package in c(suggests, "cli"))
    requireNamespace(package, quietly = TRUE)

}

renv_tests_init <- function() {
  renv_tests_init_envvars()
  renv_tests_init_repos()
  renv_tests_init_packages()
}
