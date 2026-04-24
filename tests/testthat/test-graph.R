
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
  records <- renv_graph_install(descriptions)

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
  records <- renv_graph_install(descriptions)

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

# adjacency graph ----

test_that("renv_graph_adjacency computes correct structure for chain", {

  renv_tests_scope()

  descriptions <- renv_graph_init("breakfast")
  g <- renv_graph_adjacency(descriptions)

  # breakfast -> oatmeal, toast; toast -> bread; bread, oatmeal -> nothing
  expect_true(setequal(g$packages, c("breakfast", "oatmeal", "toast", "bread")))
  expect_true("toast" %in% g$adj[["breakfast"]])
  expect_true("oatmeal" %in% g$adj[["breakfast"]])
  expect_equal(g$adj[["bread"]], character())
  expect_equal(g$adj[["oatmeal"]], character())
  expect_equal(g$adj[["toast"]], "bread")

  # in-degree: bread=0, oatmeal=0, toast=1 (from breakfast), breakfast=0 wait no
  # in-degree counts deps *within the set* for each package
  expect_equal(g$indegree[["bread"]], 0L)
  expect_equal(g$indegree[["oatmeal"]], 0L)
  expect_equal(g$indegree[["toast"]], 1L)       # depends on bread
  expect_equal(g$indegree[["breakfast"]], 2L)    # depends on oatmeal + toast

  # reverse adjacency: bread is depended on by toast; toast by breakfast
  expect_true("toast" %in% g$revadj[["bread"]])
  expect_true("breakfast" %in% g$revadj[["toast"]])
  expect_true("breakfast" %in% g$revadj[["oatmeal"]])

})

test_that("renv_graph_adjacency handles independent packages", {

  renv_tests_scope()

  descriptions <- renv_graph_init(c("bread", "egg"))
  g <- renv_graph_adjacency(descriptions)

  # no edges between independent packages
  expect_equal(g$adj[["bread"]], character())
  expect_equal(g$adj[["egg"]], character())
  expect_equal(g$indegree[["bread"]], 0L)
  expect_equal(g$indegree[["egg"]], 0L)

})

test_that("renv_graph_adjacency handles empty input", {

  g <- renv_graph_adjacency(list())
  expect_equal(length(g$packages), 0L)
  expect_equal(length(g$adj), 0L)

})

# version requirements ----

test_that("renv_graph_requirements extracts version constraints", {

  renv_tests_scope()

  # breakfast depends on toast (>= 1.0.0)
  descriptions <- renv_graph_init("breakfast")
  requirements <- renv_graph_requirements(descriptions)

  # toast should have a requirement from breakfast
  reqs <- requirements[["toast"]]
  expect_true(is.data.frame(reqs))
  expect_true(nrow(reqs) >= 1L)
  expect_true("breakfast" %in% reqs$RequiredBy)

})

test_that("renv_graph_compatible accepts satisfied constraints", {

  reqs <- data.frame(
    Package    = "toast",
    Require    = ">=",
    Version    = "1.0.0",
    RequiredBy = "breakfast",
    stringsAsFactors = FALSE
  )

  expect_true(renv_graph_compatible("1.0.0", reqs))
  expect_true(renv_graph_compatible("2.0.0", reqs))

})

test_that("renv_graph_compatible rejects unsatisfied constraints", {

  reqs <- data.frame(
    Package    = "toast",
    Require    = ">=",
    Version    = "2.0.0",
    RequiredBy = "breakfast",
    stringsAsFactors = FALSE
  )

  expect_false(renv_graph_compatible("1.0.0", reqs))
  expect_false(renv_graph_compatible("1.9.9", reqs))

})

test_that("renv_graph_compatible handles multiple constraints", {

  reqs <- data.frame(
    Package    = c("toast", "toast"),
    Require    = c(">=", ">="),
    Version    = c("1.0.0", "1.5.0"),
    RequiredBy = c("pkg1", "pkg2"),
    stringsAsFactors = FALSE
  )

  expect_true(renv_graph_compatible("1.5.0", reqs))
  expect_true(renv_graph_compatible("2.0.0", reqs))
  expect_false(renv_graph_compatible("1.2.0", reqs))

})

test_that("renv_graph_compatible returns TRUE for no requirements", {

  expect_true(renv_graph_compatible("1.0.0", NULL))
  expect_true(renv_graph_compatible("1.0.0", data.frame()))

})

# needs update ----

test_that("renv_graph_needs_update preserves installed transitive deps", {

  renv_tests_scope()

  # install bread 1.0.0 into the test library
  descriptions <- renv_graph_init("bread")
  renv_graph_install(descriptions)
  expect_true(renv_package_installed("bread"))

  # set up restore state as though "breakfast" was explicitly requested;
  # bread is a transitive dependency, not explicitly requested
  renv_scope_restore(
    project  = getwd(),
    library  = renv_libpaths_active(),
    packages = "breakfast"
  )

  # simulate graph resolution having resolved bread to a newer version
  record <- list(Package = "bread", Version = "2.0.0", Source = "Repository")

  # requirements that bread 1.0.0 satisfies
  requirements <- new.env(parent = emptyenv())
  requirements[["bread"]] <- data.frame(
    Package    = "bread",
    Require    = ">=",
    Version    = "1.0.0",
    RequiredBy = "toast",
    stringsAsFactors = FALSE
  )

  # installed bread 1.0.0 satisfies >= 1.0.0, so no update needed
  expect_false(renv_graph_needs_update("bread", record, requirements))

})

test_that("renv_graph_needs_update upgrades when requirements not satisfied", {

  renv_tests_scope()

  # install bread 1.0.0
  descriptions <- renv_graph_init("bread")
  renv_graph_install(descriptions)

  renv_scope_restore(
    project  = getwd(),
    library  = renv_libpaths_active(),
    packages = "breakfast"
  )

  record <- list(Package = "bread", Version = "2.0.0", Source = "Repository")

  # requirements that bread 1.0.0 does NOT satisfy
  requirements <- new.env(parent = emptyenv())
  requirements[["bread"]] <- data.frame(
    Package    = "bread",
    Require    = ">=",
    Version    = "2.0.0",
    RequiredBy = "toast",
    stringsAsFactors = FALSE
  )

  # installed bread 1.0.0 doesn't satisfy >= 2.0.0, so update is needed
  expect_true(renv_graph_needs_update("bread", record, requirements))

})

test_that("renv_graph_needs_update always updates explicitly-requested packages", {

  renv_tests_scope()

  # install bread 1.0.0
  descriptions <- renv_graph_init("bread")
  renv_graph_install(descriptions)

  # bread is explicitly requested this time
  renv_scope_restore(
    project  = getwd(),
    library  = renv_libpaths_active(),
    packages = "bread"
  )

  record <- list(Package = "bread", Version = "2.0.0", Source = "Repository")

  # requirements that bread 1.0.0 satisfies
  requirements <- new.env(parent = emptyenv())
  requirements[["bread"]] <- data.frame(
    Package    = "bread",
    Require    = ">=",
    Version    = "1.0.0",
    RequiredBy = "toast",
    stringsAsFactors = FALSE
  )

  # even though 1.0.0 satisfies requirements, bread was explicitly
  # requested so it should be updated
  expect_true(renv_graph_needs_update("bread", record, requirements))

})

test_that("renv_graph_needs_update returns TRUE when package not installed", {

  renv_tests_scope()

  renv_scope_restore(
    project  = getwd(),
    library  = renv_libpaths_active(),
    packages = "breakfast"
  )

  record <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  requirements <- new.env(parent = emptyenv())

  # bread is not installed, so it needs to be installed
  expect_true(renv_graph_needs_update("bread", record, requirements))

})

# install result parsing ----

test_that("renv_graph_install_parse_result handles NULL data", {

  elapsed <- as.difftime(1, units = "secs")
  result <- renv_graph_install_parse_result(NULL, elapsed)

  expect_false(result$success)
  expect_true(grepl("unexpectedly", result$output))
  expect_equal(result$elapsed, elapsed)

})

test_that("renv_graph_install_parse_result handles error object", {

  elapsed <- as.difftime(2, units = "secs")
  err <- simpleError("something went wrong")
  result <- renv_graph_install_parse_result(err, elapsed)

  expect_false(result$success)
  expect_equal(result$output, "something went wrong")
  expect_equal(result$elapsed, elapsed)

})

test_that("renv_graph_install_parse_result handles successful output", {

  elapsed <- as.difftime(3, units = "secs")
  output <- c("* installing *source* package 'bread' ...", "* DONE (bread)")
  attr(output, "status") <- 0L
  result <- renv_graph_install_parse_result(output, elapsed)

  expect_true(result$success)
  expect_equal(result$output, output)

})

test_that("renv_graph_install_parse_result handles failed output", {

  elapsed <- as.difftime(4, units = "secs")
  output <- c("ERROR: compilation failed")
  attr(output, "status") <- 1L
  result <- renv_graph_install_parse_result(output, elapsed)

  expect_false(result$success)
  expect_equal(result$output, output)

})

test_that("renv_graph_install_parse_result handles output with no status attr", {

  elapsed <- as.difftime(1, units = "secs")
  output <- c("some output")
  result <- renv_graph_install_parse_result(output, elapsed)

  # no status attribute means success (status 0)
  expect_true(result$success)

})

# install classification ----

test_that("renv_graph_install_classify inspects archive contents", {

  # the classifier must not trust the record's "type" attribute;
  # for PPM "binary" repositories, the attribute reflects the
  # requested type, not the real contents of the archive.
  path <- renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")

  record <- list(Package = "skeleton", Path = path)
  expect_equal(renv_graph_install_classify(record), "source")

  # a misleading "binary" tag must not override the real type
  attr(record, "type") <- "binary"
  expect_equal(renv_graph_install_classify(record), "source")

})

# install needs unpack ----

test_that("renv_graph_install_needs_unpack returns TRUE for RemoteSubdir", {

  record <- list(
    Package      = "mypkg",
    Path         = "/tmp/mypkg.tar.gz",
    RemoteSubdir = "subdir"
  )
  expect_true(renv_graph_install_needs_unpack(record, "source"))

})

test_that("renv_graph_install_needs_unpack returns FALSE for simple tar.gz source", {

  archive <- renv_scope_tempfile("renv-test-", fileext = ".tar.gz")
  file.create(archive)

  record <- list(Package = "mypkg", Path = archive)
  renv_scope_options(renv.config.install.build = FALSE)
  expect_false(renv_graph_install_needs_unpack(record, "source"))

})

# install error reporting ----

test_that("renv_graph_install_errors reports direct failures", {

  renv_scope_options(renv.verbose = TRUE, renv.caution.verbose = TRUE)

  descriptions <- list(
    bread = list(Package = "bread", Version = "1.0.0")
  )

  errors <- list(
    list(package = "bread", message = "compilation failed")
  )

  output <- capture.output(
    renv_graph_install_errors(errors, "bread", descriptions)
  )

  output <- paste(output, collapse = "\n")
  expect_true(grepl("bread", output))
  expect_true(grepl("compilation failed", output))

})

test_that("renv_graph_install_errors reports dependency cascade failures", {

  renv_scope_options(renv.verbose = TRUE, renv.caution.verbose = TRUE)

  descriptions <- list(
    bread = list(Package = "bread", Version = "1.0.0"),
    toast = list(Package = "toast", Version = "1.0.0", Depends = "bread")
  )

  errors <- list(
    list(package = "bread", message = "compilation failed")
  )
  failed <- c("bread", "toast")

  output <- capture.output(
    renv_graph_install_errors(errors, failed, descriptions)
  )

  output <- paste(output, collapse = "\n")
  expect_true(grepl("bread", output))
  expect_true(grepl("toast", output))
  expect_true(grepl("dependency failed", output))

})

test_that("renv_graph_install_errors is silent with no errors", {

  output <- capture.output(
    renv_graph_install_errors(list(), character(), list())
  )
  expect_equal(length(output), 0L)

})

# wave cycle detection ----

test_that("renv_graph_waves warns on dependency cycle", {

  # synthetic descriptions that form a cycle: A -> B -> A
  descriptions <- list(
    A = list(Package = "A", Version = "1.0.0", Depends = "B"),
    B = list(Package = "B", Version = "1.0.0", Depends = "A")
  )

  expect_warning(
    waves <- renv_graph_waves(descriptions),
    "dependency cycle"
  )

  # all packages should still appear
  all_pkgs <- unlist(waves)
  expect_true(setequal(all_pkgs, c("A", "B")))

})

test_that("renv_graph_waves handles empty input", {

  waves <- renv_graph_waves(list())
  expect_equal(waves, list())

})

# install pipeline integration tests ----

test_that("renv_graph_install installs multiple independent packages", {

  renv_tests_scope()

  # bread and egg have no dependency relationship
  descriptions <- renv_graph_init(c("bread", "egg"))
  records <- renv_graph_install(descriptions)

  expect_true(setequal(names(records), c("bread", "egg")))
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("egg"))

})

test_that("renv_graph_install skips already-installed packages", {

  renv_tests_scope()

  # install bread first
  descriptions <- renv_graph_init("bread")
  renv_graph_install(descriptions)
  expect_true(renv_package_installed("bread"))

  # now install breakfast; bread should be skipped
  descriptions <- renv_graph_init("breakfast")
  records <- renv_graph_install(descriptions)

  # breakfast and its other deps should be installed
  expect_true(renv_package_installed("breakfast"))
  expect_true(renv_package_installed("toast"))
  expect_true(renv_package_installed("oatmeal"))

})

test_that("renv_graph_install returns empty list for empty input", {

  renv_tests_scope()

  records <- renv_graph_install(list())
  expect_equal(records, list())

})

test_that("renv_graph_install handles deeper dependency chain", {

  renv_tests_scope()

  # jamie -> kevin + phone; kevin -> phone
  # three levels: phone -> kevin -> jamie
  descriptions <- renv_graph_init("jamie")
  records <- renv_graph_install(descriptions)

  expect_true("jamie" %in% names(records))
  expect_true("kevin" %in% names(records))
  expect_true("phone" %in% names(records))

  for (pkg in names(records))
    expect_true(renv_package_installed(pkg), info = pkg)

})

test_that("renv_graph_install with install.jobs = 1 uses sequential mode", {

  renv_tests_scope()
  renv_scope_options(renv.config.install.jobs = 1L)

  descriptions <- renv_graph_init("breakfast")
  records <- renv_graph_install(descriptions)

  expect_true(setequal(
    names(records),
    c("bread", "oatmeal", "toast", "breakfast")
  ))

  for (pkg in names(records))
    expect_true(renv_package_installed(pkg), info = pkg)

})

test_that("renv_graph_install with staged install", {

  renv_tests_scope()

  renv_scope_options(
    renv.config.install.staged = TRUE,
    renv.config.install.transactional = FALSE
  )

  descriptions <- renv_graph_init("breakfast")
  records <- renv_graph_install(descriptions)

  # all packages should be in the real library after staging
  library <- renv_libpaths_active()
  for (pkg in names(records))
    expect_true(renv_package_installed(pkg, lib.loc = library), info = pkg)

})

test_that("renv_graph_install respects dependency ordering", {

  renv_tests_scope(isolated = TRUE)

  # disable cache so all packages go through source install
  renv_scope_options(renv.config.cache.enabled = FALSE)

  # track the order packages are finalized via a tracer;
  # use a shared environment so the tracer (evaluated inside the
  # renv namespace) can write to it
  env <- new.env(parent = emptyenv())
  env$order <- character()

  renv_scope_trace(
    what = renv:::renv_graph_install_finalize,
    tracer = bquote({
      .env <- .(env)
      .env$order <- c(.env$order, record$Package)
    })
  )

  descriptions <- renv_graph_init("breakfast")
  records <- renv_graph_install(descriptions)

  # bread must be finalized before toast, toast before breakfast
  bread_idx <- match("bread", env$order)
  toast_idx <- match("toast", env$order)
  breakfast_idx <- match("breakfast", env$order)

  expect_true(!is.na(bread_idx))
  expect_true(!is.na(toast_idx))
  expect_true(!is.na(breakfast_idx))
  expect_true(bread_idx < toast_idx)
  expect_true(toast_idx < breakfast_idx)

})

test_that("binary packages respect dependency ordering", {

  skip_on_cran()
  skip_if(identical(.Platform$pkgType, "source"),
          "binary packages not supported on this platform")

  renv_tests_scope(isolated = TRUE)

  # use binary packages from the test repository
  renv_scope_options(pkgType = .Platform$pkgType)
  renv_scope_binding(the, "install_pkg_type", "binary")

  # disable cache so all packages go through binary install
  renv_scope_options(renv.config.cache.enabled = FALSE)

  # track the order packages are finalized via a tracer
  env <- new.env(parent = emptyenv())
  env$order <- character()

  renv_scope_trace(
    what = renv:::renv_graph_install_finalize,
    tracer = bquote({
      .env <- .(env)
      .env$order <- c(.env$order, record$Package)
    })
  )

  descriptions <- renv_graph_init("breakfast")
  records <- renv_graph_install(descriptions)

  # all packages should be installed and loadable
  for (pkg in names(records))
    expect_true(renv_package_installed(pkg), info = pkg)

  # bread must be finalized before toast, toast before breakfast
  bread_idx <- match("bread", env$order)
  toast_idx <- match("toast", env$order)
  breakfast_idx <- match("breakfast", env$order)

  expect_true(!is.na(bread_idx))
  expect_true(!is.na(toast_idx))
  expect_true(!is.na(breakfast_idx))
  expect_true(bread_idx < toast_idx)
  expect_true(toast_idx < breakfast_idx)

})

# graph sort edge cases ----

test_that("renv_graph_sort handles empty input", {

  sorted <- renv_graph_sort(list())
  expect_equal(length(sorted), 0L)

})

test_that("renv_graph_sort produces stable ordering for independent packages", {

  renv_tests_scope()

  descriptions <- renv_graph_init(c("bread", "egg", "oatmeal"))
  sorted <- renv_graph_sort(descriptions)

  # all three should appear; order among independent packages
  # is deterministic (alphabetical from the queue)
  expect_equal(length(sorted), 3L)
  expect_true(setequal(names(sorted), c("bread", "egg", "oatmeal")))

})

# graph deps ----

test_that("renv_graph_deps extracts dependencies from Depends/Imports/LinkingTo", {

  desc <- list(
    Package   = "mypkg",
    Depends   = "R (>= 3.5), bread, oatmeal",
    Imports   = "toast",
    LinkingTo = "egg"
  )

  deps <- renv_graph_deps(desc)

  # R should be excluded (base package)
  expect_false("R" %in% deps)
  expect_true(setequal(deps, c("bread", "oatmeal", "toast", "egg")))

})

test_that("renv_graph_deps respects custom fields argument", {

  desc <- list(
    Package  = "mypkg",
    Depends  = "bread",
    Imports  = "toast",
    Suggests = "egg"
  )

  # default fields don't include Suggests
  deps_default <- renv_graph_deps(desc)
  expect_false("egg" %in% deps_default)

  # custom fields can include Suggests
  deps_custom <- renv_graph_deps(desc, fields = c("Depends", "Imports", "Suggests"))
  expect_true("egg" %in% deps_custom)

})

test_that("renv_graph_deps handles list-valued dependency fields from v2 lockfiles", {

  # v2 lockfile records store dependency fields as lists (JSON arrays)
  # rather than comma-separated strings; renv_graph_deps should handle both
  desc <- list(
    Package   = "mypkg",
    Imports   = list("Rcpp", "data.table", "jsonlite"),
    LinkingTo = list("Rcpp", "BH")
  )

  deps <- renv_graph_deps(desc)
  expect_true(setequal(deps, c("Rcpp", "data.table", "jsonlite", "BH")))

})

test_that("renv_graph_deps returns empty for package with no deps", {

  desc <- list(Package = "mypkg", Version = "1.0.0")
  deps <- renv_graph_deps(desc)
  expect_equal(deps, character())

})

# graph URLs for multiple sources ----

test_that("renv_graph_urls resolves URLs for full dependency tree", {

  renv_tests_scope()

  descriptions <- renv_graph_init("breakfast")
  urls <- renv_graph_urls(descriptions)

  # every package should have a resolved URL (they're all from the test repo)
  for (pkg in names(descriptions)) {
    info <- urls[[pkg]]
    expect_true(is.list(info), info = pkg)
    expect_true(nzchar(info$url), info = pkg)
  }

})

test_that("renv_graph_urls gracefully handles mixed sources", {

  renv_tests_scope()

  descriptions <- list(
    bread = list(Package = "bread", Version = "1.0.0", Source = "Repository"),
    fake  = list(Package = "fake", Version = "1.0.0", Source = "unknown_source")
  )

  urls <- renv_graph_urls(descriptions)

  # bread should resolve; fake should be NULL
  expect_true(is.list(urls[["bread"]]))
  expect_null(urls[["fake"]])

})

# https://github.com/rstudio/renv/issues/2264
test_that("renv_graph_description_repository respects install_pkg_type", {

  renv_tests_scope()

  # track the type argument passed to renv_available_packages_entry
  env <- new.env(parent = emptyenv())
  env$types <- character()

  renv_scope_trace(
    what   = renv:::renv_available_packages_entry,
    tracer = bquote({
      .env <- .(env)
      .env$types <- c(.env$types, type)
    })
  )

  record <- list(Package = "bread", Source = "Repository")

  # default: install_pkg_type is NULL, should fall back to "source"
  renv_graph_description_repository(record)
  expect_true("source" %in% env$types)

  # when install_pkg_type is set, it should be forwarded
  env$types <- character()
  renv_scope_binding(the, "install_pkg_type", "binary")
  catch(renv_graph_description_repository(record))
  expect_true("binary" %in% env$types)
  expect_false("source" %in% env$types)

})

# https://github.com/rstudio/renv/issues/2278
test_that("repository graph prefers crandb over latest-with-overridden-version", {

  renv_tests_scope()

  # stub crandb lookup so we don't touch the network, and so we can verify
  # which path was taken: the stub returns a marker Depends field that
  # differs from anything in the test repository
  crandb_called <- FALSE
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = "renv_graph_description_crandb",
    replacement = function(package, version) {
      crandb_called <<- TRUE
      list(Package = package, Version = version, Depends = "crandb-marker")
    }
  )

  # ask for bread at a version that isn't in the test repository (which has
  # 1.0.0); step 1 (version-filtered entry) and step 2 (latest matching) will
  # both fail, so resolution must fall through to crandb
  record <- list(Package = "bread", Version = "0.1.0", Source = "Repository")
  desc <- renv_graph_description_repository(record)

  expect_true(crandb_called)
  expect_equal(desc$Version, "0.1.0")
  expect_equal(desc$Depends, "crandb-marker")

})

# https://github.com/rstudio/renv/issues/2249
test_that("gitlab DESCRIPTION path handles empty RemoteSubdir", {

  # when subdir is empty or NULL, the encoded path should be just "DESCRIPTION"
  # (not "%2FDESCRIPTION" which results from pasting "" with "DESCRIPTION")
  for (subdir in list(NULL, "")) {
    parts <- c(if (nzchar(subdir %||% "")) subdir, "DESCRIPTION")
    descpath <- URLencode(paste(parts, collapse = "/"), reserved = TRUE)
    expect_equal(descpath, "DESCRIPTION")
  }

  # when subdir is non-empty, it should appear in the path
  parts <- c(if (nzchar("src" %||% "")) "src", "DESCRIPTION")
  descpath <- URLencode(paste(parts, collapse = "/"), reserved = TRUE)
  expect_equal(descpath, "src%2FDESCRIPTION")

})
