
# NOTE: can't use renv_namespace_version() until the following commit is on CRAN
# https://github.com/r-lib/pkgload/commit/907749a8efaf8fe65cfde4f4ade6e7c506f1afa5
test_that("renv itself doesn't mark itself as embedded", {
  expect_false(renv_metadata_embedded())
  expect_equal(renv_metadata_version(), renv_package_version("renv"))
})

test_that("renv can be vendored into an R package", {
  skip_on_cran()
  skip_slow()

  # create a dummy R package
  project <- renv_tests_scope()

  desc <- heredoc("
    Type: Package
    Package: test.renv.embedding
    Version: 0.1.0
  ")

  writeLines(desc, con = "DESCRIPTION")
  file.create("NAMESPACE")

  # vendor renv
  vendor()

  # make sure renv is initializes in .onLoad()
  code <- heredoc('
    .onLoad <- function(libname, pkgname) {
      renv$initialize()
    }
  ')

  ensure_directory("R")
  writeLines(code, con = "R/zzz.R")

  # try installing the package
  r_cmd_install("test.renv.embedding", getwd())

  # test that we can load the package and initialize renv
  code <- substitute({

    # make sure renv isn't visible on library paths
    base <- .BaseNamespaceEnv
    base$.libPaths(path)

    # extra sanity check
    if (requireNamespace("renv", quietly = TRUE))
      stop("internal error: renv shouldn't be visible on library paths")

    # load the package, and check that renv realizes it's embedded
    namespace <- base$asNamespace("test.renv.embedding")
    embedded <- namespace$renv$renv_metadata_embedded()
    if (!embedded)
      stop("internal error: renv is embedded but doesn't realize it")

    # let parent process know we succeeded
    writeLines(as.character(embedded))

  }, list(path = .libPaths()[1]))

  script <- renv_scope_tempfile("renv-script-", fileext = ".R")
  writeLines(deparse(code), con = script)

  # attempt to run script
  output <- renv_system_exec(R(), c("--vanilla", "-s", "-f", renv_shell_path(script)))
  expect_equal(output, "TRUE")

  # test that we can use the embedded renv to run snapshot
  code <- substitute({

    # make sure renv isn't visible on library paths
    base <- .BaseNamespaceEnv
    base$.libPaths(path)

    # try to list
    ns <- base$asNamespace("test.renv.embedding")
    deps <- ns$renv$dependencies()
    saveRDS(deps, file = "dependencies.rds")

  }, list(path = .libPaths()[1]))

  script <- renv_scope_tempfile("renv-script-", fileext = ".R")
  writeLines(deparse(code), con = script)

  # attempt to run script
  output <- renv_system_exec(R(), c("--vanilla", "-s", "-f", renv_shell_path(script)), quiet = FALSE)
  expect_true(file.exists("dependencies.rds"))

})
