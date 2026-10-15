
# Local packages let us exercise Remotes without network access. In each
# description, Remotes names another package in this fixture.
renv_tests_graph_remotes <- function(descriptions, scope = parent.frame()) {

  root <- renv_scope_tempfile("renv-remotes-", scope = scope)
  paths <- setNames(file.path(root, names(descriptions)), names(descriptions))
  ensure_directory(paths)

  for (package in names(descriptions)) {
    desc <- c(list(Package = package, Version = "1.0.0", Type = "Package"),
              descriptions[[package]])
    if (length(desc$Remotes))
      desc$Remotes <- paste(paths[desc$Remotes], collapse = ", ")
    renv_dcf_write(desc, file = file.path(paths[[package]], "DESCRIPTION"))
    writeLines("", con = file.path(paths[[package]], "NAMESPACE"))
  }

  paths

}

test_that("restore ignores optional Remotes and their dependencies", {

  project <- renv_tests_scope()
  renv_scope_options(renv.verbose = TRUE, renv.caution.verbose = TRUE)
  paths <- renv_tests_graph_remotes(list(
    bagel = list(Imports = "bread", Suggests = "cream",
                 Remotes = c("bread", "cream", "unused")),
    bread = list(Suggests = "cream", Remotes = "cream"),
    cream = list(Imports = "jam", Remotes = "jam"),
    jam = list(),
    unused = list()
  ))

  # Only bagel and bread are recorded; cream and unused are optional.
  lockfile <- renv_lockfile_init(project)
  lockfile$Packages <- map(paths[c("bagel", "bread")], renv_remotes_resolve)
  renv_lockfile_write(lockfile, "renv.lock")

  output <- capture.output(records <- restore(prompt = FALSE))
  expect_setequal(names(records), c("bagel", "bread"))
  expect_setequal(list.files(renv_libpaths_active()), c("bagel", "bread"))
  expect_false(any(grepl("dependency tree was repaired", output, fixed = TRUE)))
  expect_equal(renv_lockfile_read("renv.lock")$Packages, lockfile$Packages)

})

test_that("restore repairs required dependencies using Remotes", {

  project <- renv_tests_scope()
  renv_scope_options(renv.verbose = TRUE, renv.caution.verbose = TRUE)
  paths <- renv_tests_graph_remotes(list(
    bagel = list(Imports = "bread", Suggests = "cream",
                 Remotes = c("bread", "cream")),
    bread = list(Imports = "jam", Remotes = "jam"),
    jam = list(),
    cream = list()
  ))

  # The lockfile is incomplete: bread and its dependency jam are missing.
  lockfile <- renv_lockfile_init(project)
  lockfile$Packages <- map(paths["bagel"], renv_remotes_resolve)
  renv_lockfile_write(lockfile, "renv.lock")

  output <- capture.output(records <- restore(prompt = FALSE))
  expect_setequal(names(records), c("bagel", "bread", "jam"))
  expect_true(any(grepl("dependency tree was repaired", output, fixed = TRUE)))
  expect_equal(renv_description_read(package = "bread")$RemoteUrl, paths[["bread"]])
  expect_equal(renv_description_read(package = "jam")$RemoteUrl, paths[["jam"]])
  expect_false(renv_package_installed("cream"))

})

test_that("install includes remote Suggests only when requested", {

  renv_tests_scope()
  paths <- renv_tests_graph_remotes(list(
    bagel = list(Suggests = "cream", Remotes = "cream"),
    cream = list(Imports = "jam", Suggests = "unused",
                 Remotes = c("jam", "unused")),
    jam = list(),
    unused = list()
  ))

  records <- install(paths[["bagel"]], prompt = FALSE)
  expect_setequal(names(records), "bagel")
  expect_false(renv_package_installed("cream"))

  records <- install(paths[["bagel"]], dependencies = TRUE, prompt = FALSE)
  expect_setequal(names(records), c("bagel", "cream", "jam"))
  expect_equal(renv_description_read(package = "cream")$RemoteUrl, paths[["cream"]])
  expect_true(renv_package_installed("jam"))
  expect_false(renv_package_installed("unused"))

})

test_that("restore respects explicitly configured Suggests with Remotes", {

  project <- renv_tests_scope()
  paths <- renv_tests_graph_remotes(list(
    bagel = list(Suggests = "cream", Remotes = "cream"),
    cream = list()
  ))

  settings$package.dependency.fields(c("Imports", "Suggests"))
  lockfile <- renv_lockfile_init(project)
  lockfile$Packages <- map(paths["bagel"], renv_remotes_resolve)
  records <- restore(lockfile = lockfile, prompt = FALSE)

  expect_setequal(names(records), c("bagel", "cream"))
  expect_equal(renv_description_read(package = "cream")$RemoteUrl, paths[["cream"]])

})

test_that("a package's Remotes can provide an indirect dependency", {

  renv_tests_scope()
  paths <- renv_tests_graph_remotes(list(
    bagel = list(Imports = "toast", Remotes = c("toast", "bread")),
    toast = list(Imports = "bread"),
    bread = list()
  ))

  descriptions <- renv_graph_init(paths[["bagel"]])
  expect_setequal(names(descriptions), c("bagel", "toast", "bread"))
  expect_equal(descriptions$bread$RemoteUrl, paths[["bread"]])

})

test_that("unused remote packages cannot override required dependency sources", {

  renv_tests_scope()
  paths <- renv_tests_graph_remotes(list(
    bagel = list(Imports = "bread", Suggests = "cream", Remotes = "cream"),
    cream = list(Remotes = "bread"),
    bread = list()
  ))

  descriptions <- renv_graph_init(paths[["bagel"]])
  expect_setequal(names(descriptions), c("bagel", "bread"))
  expect_equal(descriptions$bread$Source, "Repository")
  expect_null(descriptions$bread$RemoteUrl)

})

test_that("later Remotes override selected repository packages without losing Suggests", {

  renv_tests_scope()
  paths <- renv_tests_graph_remotes(list(
    bagel = list(Remotes = "bread"),
    bread = list(Suggests = "cream", Remotes = "cream"),
    cream = list()
  ))

  # bread is selected before bagel supplies its remote; it is still a root,
  # so the requested Suggests must apply to its replacement description.
  renv_scope_binding(the, "install_dependency_fields",
                     c("Depends", "Imports", "LinkingTo", "Suggests"))
  descriptions <- renv_graph_init(c("bread", paths[["bagel"]]))
  expect_setequal(names(descriptions), c("bagel", "bread", "cream"))
  expect_equal(descriptions$bread$RemoteUrl, paths[["bread"]])
  expect_equal(descriptions$cream$RemoteUrl, paths[["cream"]])

})
