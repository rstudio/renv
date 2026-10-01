
test_that("renv itself doesn't mark itself as embedded", {
  expect_false(renv_metadata_embedded())
  expect_equal(
    c(renv_metadata_version()),
    c(renv_namespace_version("renv"))
  )
})

test_that("system.file() falls through for resources vendor() doesn't bundle", {

  # renv has no 'inst/vendor' directory of its own, so with 'embedded' set
  # the vendored lookup fails and we should fall back to the regular lookup
  metadata <- renv_metadata_create(embedded = TRUE, version = the$metadata$version)
  renv_scope_binding(the, "metadata", metadata)

  schema <- system.file(
    "schema",
    "draft-07.renv.lock.schema.json",
    package  = "renv",
    mustWork = TRUE
  )

  expect_true(file.exists(schema))

})

test_that("vendor() skips resources older versions of renv don't have", {

  sources <- renv_scope_tempfile("renv-sources-")
  askpass <- file.path(sources, "inst/resources/scripts-git-askpass.sh")
  ensure_parent_directory(askpass)
  file.create(askpass)

  project <- renv_scope_tempfile("renv-project-")
  ensure_directory(project)

  resources <- renv_vendor_resources(project, sources)
  expect_equal(resources, file.path(project, "inst/vendor"))
  expect_true(file.exists(file.path(resources, "resources/scripts-git-askpass.sh")))
  expect_false(file.exists(file.path(resources, "sysreqs/sysreqs.json")))

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

  # vendor the sources under test, rather than the latest sources on GitHub
  sources <- renv_tests_vendor_sources()
  if (!is.null(sources)) {

    renv_scope_binding(
      envir       = asNamespace("renv"),
      symbol      = "renv_vendor_sources",
      replacement = function(version) sources
    )

    remote <- renv_tests_vendor_remote(sources)
    renv_scope_binding(
      envir       = asNamespace("renv"),
      symbol      = "renv_remotes_resolve",
      replacement = function(spec, ...) remote
    )

  }

  # vendor renv
  vendor()

  # the resources renv reads at runtime should be bundled alongside
  expect_true(file.exists("inst/vendor/sysreqs/sysreqs.json"))
  expect_true(file.exists("inst/vendor/resources/scripts-git-askpass.sh"))

  # make sure renv is initializes in .onLoad()
  code <- heredoc('
    .onLoad <- function(libname, pkgname) {
      renv$initialize(libname, pkgname)
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

  # test that the embedded renv finds its own resources, and can resolve
  # `renv::use()` calls, without an installed copy of renv
  writeLines("renv::use(digest = \"eddelbuettel/digest\")", con = "use.R")

  code <- substitute({

    # make sure renv isn't visible on library paths
    base <- .BaseNamespaceEnv
    base$.libPaths(path)

    ns <- base$asNamespace("test.renv.embedding")
    result <- list(
      rules   = ns$renv$system.file("sysreqs/sysreqs.json", package = "renv"),
      askpass = ns$renv$system.file("resources/scripts-git-askpass.sh", package = "renv"),
      nrules  = length(ns$renv$renv_sysreqs_rules()),
      deps    = ns$renv$dependencies("use.R", quiet = TRUE)$Package
    )

    saveRDS(result, file = "embedded.rds")

  }, list(path = .libPaths()[1]))

  script <- renv_scope_tempfile("renv-script-", fileext = ".R")
  writeLines(deparse(code), con = script)

  output <- renv_system_exec(R(), c("--vanilla", "-s", "-f", renv_shell_path(script)), quiet = FALSE)
  result <- readRDS("embedded.rds")

  expect_true(file.exists(result$rules))
  expect_true(file.exists(result$askpass))
  expect_true(result$nrules > 0L)
  expect_true("digest" %in% result$deps)

})
