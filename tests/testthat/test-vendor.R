
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

test_that("vendor() copies whatever 'inst' the sources provide", {

  # older versions of renv lack some resources; those shouldn't be an error
  sources <- renv_scope_tempfile("renv-sources-")
  askpass <- file.path(sources, "inst/resources/scripts-git-askpass.sh")
  ensure_parent_directory(askpass)
  file.create(askpass)

  project <- renv_scope_tempfile("renv-project-")
  ensure_directory(project)

  # files left over from a previous vendor() should be replaced
  stale <- file.path(project, "inst/vendor/resources/stale.R")
  ensure_parent_directory(stale)
  file.create(stale)

  resources <- renv_vendor_resources(project, sources)
  expect_equal(resources, file.path(project, "inst/vendor"))
  expect_true(file.exists(file.path(resources, "resources/scripts-git-askpass.sh")))
  expect_false(file.exists(file.path(resources, "sysreqs/sysreqs.json")))
  expect_false(file.exists(stale))

})

test_that("vendor() leaves out files excluded from a build of renv", {

  sources <- renv_scope_tempfile("renv-sources-")
  ensure_directory(file.path(sources, "inst/ext"))
  file.create(file.path(sources, "inst/ext/renv.c"))
  file.create(file.path(sources, "inst/ext/.clang-format"))
  writeLines(c("^docs$", "", "\\.clang-format$"), con = file.path(sources, ".Rbuildignore"))

  project <- renv_scope_tempfile("renv-project-")
  ensure_directory(project)

  resources <- renv_vendor_resources(project, sources)
  expect_true(file.exists(file.path(resources, "ext/renv.c")))
  expect_false(file.exists(file.path(resources, "ext/.clang-format")))

})

test_that("an embedded renv uses the system.file() shim for its host package", {

  # pkgload places its shims in the imports environment of the package it
  # loads, which is the host package when renv is embedded
  imports <- new.env(parent = baseenv())
  host <- new.env(parent = imports)
  host$.packageName <- "host"
  embedded <- new.env(parent = new.env(parent = host))

  expect_identical(renv_mask_system_file(embedded), base::system.file)

  # a system.file() defined by the host package itself isn't a shim
  host$system.file <- function(...) "host"
  expect_identical(renv_mask_system_file(embedded), base::system.file)

  shim <- function(...) "shim"
  imports$system.file <- shim
  expect_identical(renv_mask_system_file(embedded), shim)

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

  # renv's 'inst' directory should be bundled alongside the vendored script
  expect_true(file.exists("inst/vendor/renv.R"))
  expect_true(file.exists("inst/vendor/sysreqs/sysreqs.json"))
  expect_true(file.exists("inst/vendor/resources/activate.R"))
  expect_true(file.exists("inst/vendor/resources/scripts-git-askpass.sh"))
  expect_true(file.exists("inst/vendor/schema/draft-07.renv.lock.schema.json"))

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
      pkgpath = base$system.file(package = "test.renv.embedding"),
      rules   = ns$renv$system.file("sysreqs/sysreqs.json", package = "renv"),
      askpass = ns$renv$system.file("resources/scripts-git-askpass.sh", package = "renv"),
      schema  = ns$renv$system.file("schema", "draft-07.renv.lock.schema.json", package = "renv", mustWork = TRUE),
      nrules  = length(ns$renv$renv_sysreqs_rules()),
      deps    = ns$renv$dependencies("use.R", quiet = TRUE)$Package
    )

    saveRDS(result, file = "embedded.rds")

  }, list(path = .libPaths()[1]))

  script <- renv_scope_tempfile("renv-script-", fileext = ".R")
  writeLines(deparse(code), con = script)

  output <- renv_system_exec(R(), c("--vanilla", "-s", "-f", renv_shell_path(script)), quiet = FALSE)
  result <- readRDS("embedded.rds")

  # the paths should point into the host package, not an installed renv
  # (which may well be visible in the library used by the subprocess)
  vendor <- file.path(result$pkgpath, "vendor")
  expect_true(startsWith(result$rules, vendor))
  expect_true(startsWith(result$askpass, vendor))
  expect_true(startsWith(result$schema, vendor))

  expect_true(file.exists(result$rules))
  expect_true(file.exists(result$askpass))
  expect_true(file.exists(result$schema))
  expect_true(result$nrules > 0L)
  expect_true("digest" %in% result$deps)

  # the embedded renv should find its resources as well when the package is
  # loaded from its sources with pkgload, where they still live under 'inst'
  skip_if_not_installed("pkgload")

  code <- substitute({

    # make sure pkgload, and the packages it uses, can be found
    base <- .BaseNamespaceEnv
    base$.libPaths(c(library, base$.libPaths()))

    pkgload::load_all(quiet = TRUE)
    ns <- base$asNamespace("test.renv.embedding")
    rules <- ns$renv$system.file("sysreqs/sysreqs.json", package = "renv")
    writeLines(rules, con = "loaded.txt")

  }, list(library = dirname(renv_namespace_path("pkgload"))))

  script <- renv_scope_tempfile("renv-script-", fileext = ".R")
  writeLines(deparse(code), con = script)

  # the path should point into the package sources, rather than into an
  # installed copy of the package, or an installed renv
  output <- renv_system_exec(R(), c("--vanilla", "-s", "-f", renv_shell_path(script)), quiet = FALSE)
  rules <- readLines("loaded.txt")
  expect_true(renv_file_same(rules, "inst/vendor/sysreqs/sysreqs.json"))

})
