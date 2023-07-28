
the$tests_root <- NULL

# NOTE: Prefer using 'is_testing()' to 'renv_tests_running()' for behavior
# that should apply regardless of the package currently being tested.
#
# renv_tests_running() is appropriate when running renv's own tests.
renv_tests_running <- function() {
  getOption("renv.tests.running", default = FALSE)
}

renv_test_code <- function(code, data = list(), fileext = ".R", scope = parent.frame()) {
  code <- do.call(substitute, list(substitute(code), data))
  file <- renv_scope_tempfile("renv-code-", fileext = fileext, scope = scope)

  writeLines(deparse(code), con = file)
  file
}

renv_test_retrieve <- function(record) {

  renv_scope_error_handler()

  # avoid using cache
  cache <- renv_scope_tempfile()
  renv_scope_envvars(RENV_PATHS_CACHE = cache)

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
    recursive = TRUE
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
  caution_bullets(
    "The following R libraries are set:",
    paste("-", .libPaths())
  )

  # print repositories
  repos <- getOption("repos")
  caution_bullets(
    "The following repositories are set:",
    paste(names(repos), repos, sep = ": ")
  )

  # print renv root
  caution_bullets(
    "The following renv root directory is being used:",
    paste("-", paths$root())
  )

  # print cache root
  caution_bullets(
    "The following renv cache directory is being used:",
    paste("-", paths$cache())
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

  caution_bullets(
    "The following PATH is set:",
    paste("-", splat)
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

  caution_bullets(
    "The following environment variables of interest are set:",
    paste(keys, vals, sep = " : ")
  )

}

renv_tests_root <- function() {
  the$tests_root <- the$tests_root %||% {
    renv_path_normalize(testthat::test_path("."))
  }
}

renv_tests_path <- function(path = NULL) {

  # special case for NULL path
  if (is.null(path))
    return(renv_tests_root())

  # otherwise, form path from root
  file.path(renv_tests_root(), path)

}

renv_tests_supported <- function() {

  # supported when running locally + on CI
  for (envvar in c("NOT_CRAN", "CI"))
    if (renv_envvar_exists(envvar))
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
