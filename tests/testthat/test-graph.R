
test_that("renv_graph_init resolves a package with no dependencies", {

  renv_tests_scope()

  descriptions <- renv_graph_init("bread")
  expect_true("bread" %in% names(descriptions))
  expect_equal(descriptions[["bread"]]$Package, "bread")
  expect_equal(length(descriptions), 1L)

})

test_that("renv_graph_init resolves transitive dependencies", {

  renv_tests_scope()

  descriptions <- renv_graph_init("breakfast")
  packages <- names(descriptions)

  # breakfast depends on oatmeal and toast; toast depends on bread
  expect_true("breakfast" %in% packages)
  expect_true("oatmeal" %in% packages)
  expect_true("toast" %in% packages)
  expect_true("bread" %in% packages)

  # suggests (egg) should not be included
  expect_false("egg" %in% packages)

})

test_that("renv_graph_sort produces a valid topological order", {

  renv_tests_scope()

  descriptions <- renv_graph_init("breakfast")
  sorted <- renv_graph_sort(descriptions)
  order <- names(sorted)

  # leaves must come before their dependents
  expect_true(which(order == "bread") < which(order == "toast"))
  expect_true(which(order == "oatmeal") < which(order == "breakfast"))
  expect_true(which(order == "toast") < which(order == "breakfast"))

})

test_that("renv_graph_sort handles packages with no dependencies", {

  renv_tests_scope()

  descriptions <- renv_graph_init("bread")
  sorted <- renv_graph_sort(descriptions)
  expect_equal(names(sorted), "bread")

})

test_that("renv_graph_download retrieves packages", {

  renv_tests_scope()

  descriptions <- renv_graph_init("breakfast")
  sorted <- renv_graph_sort(descriptions)
  records <- renv_graph_download(sorted)

  # all packages in the graph should be downloaded
  expect_true("breakfast" %in% names(records))
  expect_true("oatmeal" %in% names(records))
  expect_true("toast" %in% names(records))
  expect_true("bread" %in% names(records))

  # each record should have a Path pointing to an existing file
  for (nm in names(records)) {
    expect_true(nzchar(records[[nm]]$Path), info = nm)
    expect_true(file.exists(records[[nm]]$Path), info = nm)
  }

})

test_that("renv_graph_download retrieves a single leaf package", {

  renv_tests_scope()

  descriptions <- renv_graph_init("bread")
  records <- renv_graph_download(descriptions)

  expect_equal(names(records), "bread")
  expect_true(file.exists(records[["bread"]]$Path))

})

test_that("renv_graph_init handles multiple roots", {

  renv_tests_scope()

  descriptions <- renv_graph_init(c("bread", "egg"))

  expect_equal(sort(names(descriptions)), c("bread", "egg"))

})

test_that("renv_graph_init deduplicates shared dependencies", {

  renv_tests_scope()

  # breakfast and brunch both depend on oatmeal and toast
  descriptions <- renv_graph_init(c("breakfast", "brunch"))
  packages <- names(descriptions)

  # shared deps should appear only once (no duplicates by construction)
  expect_equal(length(packages), length(unique(packages)))

  # both roots and shared deps should be present
  expect_true("breakfast" %in% packages)
  expect_true("brunch" %in% packages)
  expect_true("oatmeal" %in% packages)
  expect_true("toast" %in% packages)
  expect_true("bread" %in% packages)

})

test_that("renv_graph_waves computes correct wave structure", {

  renv_tests_scope()

  descriptions <- renv_graph_init("breakfast")
  waves <- renv_graph_waves(descriptions)

  # should have multiple waves
  expect_true(length(waves) >= 2L)

  # all packages should appear exactly once across waves
  all_pkgs <- unlist(waves)
  expect_equal(sort(all_pkgs), sort(names(descriptions)))
  expect_equal(length(all_pkgs), length(unique(all_pkgs)))

  # leaves (bread, oatmeal) must be in an earlier wave than their dependents
  wave_of <- function(pkg) {
    for (i in seq_along(waves))
      if (pkg %in% waves[[i]])
        return(i)
  }

  expect_true(wave_of("bread") < wave_of("toast"))
  expect_true(wave_of("toast") < wave_of("breakfast"))
  expect_true(wave_of("oatmeal") < wave_of("breakfast"))

})

test_that("renv_graph_waves returns single wave for leaf package", {

  renv_tests_scope()

  descriptions <- renv_graph_init("bread")
  waves <- renv_graph_waves(descriptions)

  expect_equal(length(waves), 1L)
  expect_equal(waves[[1L]], "bread")

})

test_that("renv_graph_waves groups independent packages in same wave", {

  renv_tests_scope()

  # bread and egg have no dependencies on each other
  descriptions <- renv_graph_init(c("bread", "egg"))
  waves <- renv_graph_waves(descriptions)

  # both should be in wave 1 (no deps within the set)
  expect_equal(length(waves), 1L)
  expect_equal(sort(waves[[1L]]), sort(c("bread", "egg")))

})

test_that("renv_graph_install installs packages end to end", {

  renv_tests_scope()

  descriptions <- renv_graph_init("breakfast")
  records <- renv_graph_install(descriptions, jobs = 2L)

  # all packages should be installed
  expect_true("breakfast" %in% names(records))
  expect_true("oatmeal" %in% names(records))
  expect_true("toast" %in% names(records))
  expect_true("bread" %in% names(records))

  # packages should actually be loadable
  library <- renv_libpaths_active()
  for (pkg in names(records))
    expect_true(renv_package_installed(pkg, lib.loc = library), info = pkg)

})

test_that("renv_graph_install installs a single leaf package", {

  renv_tests_scope()

  descriptions <- renv_graph_init("bread")
  records <- renv_graph_install(descriptions, jobs = 1L)

  expect_equal(names(records), "bread")
  expect_true(renv_package_installed("bread"))

})

test_that("renv_graph_urls resolves repository package URLs", {

  renv_tests_scope()

  descriptions <- renv_graph_init("bread")
  urls <- renv_graph_urls(descriptions)

  expect_true("bread" %in% names(urls))

  info <- urls[["bread"]]
  expect_true(is.list(info))
  expect_true(nzchar(info$url))
  expect_true(nzchar(info$destfile))
  expect_true(grepl("bread", info$url))

})

test_that("renv_graph_urls returns NULL for unsupported sources", {

  desc <- list(Package = "fakepkg", Version = "1.0", Source = "bitbucket")
  descriptions <- list(fakepkg = desc)

  urls <- renv_graph_urls(descriptions)
  expect_null(urls[["fakepkg"]])

})

test_that("renv_graph_url_github constructs correct URL", {

  desc <- list(
    Package        = "mypkg",
    Version        = "1.0.0",
    Source         = "GitHub",
    RemoteUsername = "owner",
    RemoteRepo     = "repo",
    RemoteSha      = "abc123"
  )

  info <- renv_graph_url_github(desc)
  expect_true(is.list(info))
  expect_true(grepl("owner/repo/tarball/abc123", info$url))
  expect_equal(info$type, "github")

})

test_that("renv_graph_url_github returns NULL when fields are missing", {

  desc <- list(Package = "mypkg", Version = "1.0.0", Source = "GitHub")
  expect_null(renv_graph_url_github(desc))

})

test_that("renv_graph_url_gitlab constructs correct URL", {

  desc <- list(
    Package        = "mypkg",
    Version        = "1.0.0",
    Source         = "GitLab",
    RemoteUsername = "user",
    RemoteRepo     = "repo",
    RemoteSha      = "def456"
  )

  info <- renv_graph_url_gitlab(desc)
  expect_true(is.list(info))
  expect_true(grepl("projects/user%2Frepo/repository/archive", info$url))
  expect_true(grepl("sha=def456", info$url))
  expect_equal(info$type, "gitlab")

})

test_that("renv_graph_url_url resolves RemoteUrl", {

  desc <- list(
    Package   = "mypkg",
    Version   = "1.0.0",
    Source    = "url",
    RemoteUrl = "https://example.com/mypkg_1.0.0.tar.gz"
  )

  info <- renv_graph_url_url(desc)
  expect_true(is.list(info))
  expect_equal(info$url, "https://example.com/mypkg_1.0.0.tar.gz")
  expect_equal(info$type, "url")
  expect_true(nzchar(info$destfile))

})

test_that("renv_graph_url_local converts file path to file URI", {

  src <- renv_scope_tempfile("renv-local-pkg-", fileext = ".tar.gz")
  file.create(src)

  desc <- list(
    Package = "mypkg",
    Version = "1.0.0",
    Source  = "local",
    Path    = src
  )

  info <- renv_graph_url_local(desc)
  expect_true(is.list(info))
  expect_true(startsWith(info$url, "file://"))
  expect_equal(info$type, "local")

})

test_that("renv_graph_url_local returns NULL when path doesn't exist", {

  desc <- list(
    Package = "mypkg",
    Version = "1.0.0",
    Source  = "local",
    Path    = "/nonexistent/path/mypkg_1.0.0.tar.gz"
  )

  expect_null(renv_graph_url_local(desc))

})

test_that("renv_graph_url_repository resolves from test repo", {

  renv_tests_scope()

  desc <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  info <- renv_graph_url_repository(desc)

  expect_true(is.list(info))
  expect_true(grepl("bread", info$url))
  expect_equal(info$type, "repository")

})

test_that("renv_graph_download uses parallel path for test packages", {

  renv_tests_scope()

  descriptions <- renv_graph_init("breakfast")
  sorted <- renv_graph_sort(descriptions)
  records <- renv_graph_download(sorted)

  for (nm in names(records)) {
    expect_true(nzchar(records[[nm]]$Path), info = nm)
    expect_true(file.exists(records[[nm]]$Path), info = nm)
  }

})
