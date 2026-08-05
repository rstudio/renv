
test_that("renv_namespace_libpath finds the library providing a namespace", {

  renv_tests_scope()

  projlib <- renv_scope_tempfile("renv-projlib-")
  ensure_directory(projlib)
  install("bread", library = projlib)

  # indirect the package name through a variable so R CMD check doesn't
  # flag bread as an undeclared dependency
  pkg <- "bread"
  renv_scope_libpaths(c(projlib, .Library))
  loadNamespace(pkg, lib.loc = projlib)
  defer(unloadNamespace(pkg))

  expect_equal(renv_namespace_libpath(pkg, projlib), projlib)

  # libpaths defaults to the active library paths
  expect_equal(renv_namespace_libpath(pkg), projlib)

})

test_that("renv_namespace_libpath resolves packages linked from the cache", {

  # https://github.com/rstudio/renv/issues/2344 -- R records a namespace's
  # path in resolved form, so the library entry and the namespace path have
  # to be normalized before they can be compared

  skip_on_os("windows")

  renv_tests_scope()

  # the real package lives outside the library, which holds only a symlink
  cache <- renv_scope_tempfile("renv-cache-")
  ensure_directory(cache)
  install("bread", library = cache)

  projlib <- renv_scope_tempfile("renv-projlib-")
  ensure_directory(projlib)

  pkg <- "bread"
  file.symlink(file.path(cache, pkg), file.path(projlib, pkg))

  renv_scope_libpaths(c(projlib, .Library))
  loadNamespace(pkg, lib.loc = projlib)
  defer(unloadNamespace(pkg))

  expect_equal(renv_namespace_libpath(pkg, projlib), projlib)

})

test_that("renv_namespace_libpath returns '' for namespaces outside the libpaths", {

  renv_tests_scope()

  userlib <- renv_scope_tempfile("renv-userlib-")
  ensure_directory(userlib)
  install("bread", library = userlib)

  pkg <- "bread"
  renv_scope_libpaths(c(userlib, .Library))
  loadNamespace(pkg, lib.loc = userlib)
  defer(unloadNamespace(pkg))

  # a library which doesn't provide bread at all
  projlib <- renv_scope_tempfile("renv-projlib-")
  ensure_directory(projlib)

  expect_equal(renv_namespace_libpath(pkg, projlib), "")

})
