
renv_tests_program <- function(name) {
  program <- Sys.which(name)
  if (!nzchar(program))
    testthat::skip(paste("required program", name, "is not available"))
  program
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
