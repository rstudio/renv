
test_that("staging follows the project library root and profile", {

  project <- renv_tests_scope()
  renv_project_set(project)
  renv_scope_envvars(RENV_PATHS_LIBRARY_STAGING = NULL)

  overrides <- c("", "RENV_PATHS_LIBRARY_ROOT", "RENV_PATHS_LIBRARY")
  for (override in overrides) {
    for (profile in c("default", "testing")) {
      local({

        renv_scope_envvars(RENV_PROFILE = profile)
        if (nzchar(override)) {
          root <- renv_scope_tempfile("renv-library-root-")
          renv_scope_envvars(list = setNames(list(root), override))
        }

        library <- renv_paths_library(project = project)
        ensure_directory(library)
        renv_scope_libpaths(library)

        staging <- renv_install_staged_library_path()
        expected <- file.path(renv_paths_library_root(project), ".renv-staging")
        expect_true(renv_path_same(dirname(staging), expected))
        expect_true(dir.exists(staging))

        other <- renv_install_staged_library_path()
        expect_false(renv_path_same(staging, other))
        expect_true(renv_path_same(dirname(other), expected))

      })
    }
  }

})

test_that("staging follows an explicit destination library", {

  project <- renv_tests_scope()
  renv_project_set(project)
  renv_scope_envvars(RENV_PATHS_LIBRARY_STAGING = NULL)

  # renv_tests_scope() supplies a library outside the project library root
  library <- renv_libpaths_active()
  staging <- renv_install_staged_library_path()
  expect_true(renv_path_same(dirname(staging), file.path(library, ".renv")))

})

test_that("staging without an active project follows the destination library", {

  renv_tests_scope()
  renv_scope_binding(the, "project_path", NULL)
  renv_scope_envvars(RENV_PATHS_LIBRARY_STAGING = NULL)

  library <- renv_libpaths_active()
  staging <- renv_install_staged_library_path()
  expect_true(renv_path_same(dirname(staging), file.path(library, ".renv")))

})

test_that("an explicit staging root takes precedence over the library", {

  project <- renv_tests_scope()
  renv_project_set(project)

  root <- renv_scope_tempfile("renv-staging-root-")
  renv_scope_envvars(RENV_PATHS_LIBRARY_STAGING = root)

  staging <- renv_install_staged_library_path()
  expect_true(renv_path_same(dirname(staging), root))

})

test_that("staging falls back to a temporary directory when its root is unusable", {

  renv_tests_scope()
  root <- renv_scope_tempfile("renv-staging-file-")
  expect_true(file.create(root))
  renv_scope_envvars(RENV_PATHS_LIBRARY_STAGING = file.path(root, "staging"))

  staging <- renv_install_staged_library_path()
  defer(unlink(staging, recursive = TRUE))
  expect_true(dir.exists(staging))
  expect_true(renv_path_same(dirname(staging), tempdir()))

})

test_that("transactional installs link to the cache with a relocated library", {

  project <- renv_tests_scope()
  renv_project_set(project)

  root <- renv_scope_tempfile("renv-library-root-")
  renv_scope_envvars(RENV_PATHS_LIBRARY_ROOT = root, RENV_PATHS_LIBRARY_STAGING = NULL)
  renv_scope_options(renv.config.cache.symlinks = TRUE)

  library <- renv_paths_library(project = project)
  ensure_directory(library)
  renv_scope_libpaths(library)

  install("bread", transactional = TRUE)

  package <- file.path(library, "bread")
  cache <- renv_cache_path(package)
  expect_true(renv_file_same(package, cache))
  expect_false(dir.exists(renv_paths_renv("staging", project = project)))

})
