
context("Embedding")

test_that("renv itself doesn't mark itself as embedded", {
  expect_false(renv_metadata_embedded())
  expect_equal(renv_metadata_version(), renv_namespace_version("renv"))
})

test_that("renv can be embedded in a separate R package", {
  skip_on_cran()

  # create a dummy R package
  renv_tests_scope()

  desc <- heredoc("
    Type: Package
    Package: test.renv.embedding
    Version: 0.1.0
  ")

  writeLines(desc, con = "DESCRIPTION")
  file.create("NAMESPACE")

  # vendor renv
  sources <- getOption("renv.test.sources")
  if (is.null(sources) || !file.exists(sources))
    skip("path to renv sources not available")

  local({
    renv_scope_sink()
    vendor(sources = sources)
  })

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

})
