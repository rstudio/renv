
test_that("available_packages() returns NULL when no repos set", {
  skip_on_cran()

  local({
    renv_scope_options(repos = character())
    expect_null(available_packages(type = "source"))
  })

  local({
    renv_scope_options(repos = list())
    expect_null(available_packages(type = "source"))
  })

  local({
    renv_scope_options(repos = NULL)
    expect_null(available_packages(type = "source"))
  })

})

test_that("renv handles multiple available source packages", {
  skip_on_cran()

  renv_tests_scope()

  dbs <- available_packages(type = "source")
  cran <- dbs[["CRAN"]]
  entries <- cran[cran$Package == "breakfast", ]
  expect_true(nrow(entries) == 3)

  entry <- renv_available_packages_entry(
    package = "breakfast",
    type    = "source"
  )

  expect_true(nrow(entry) == 1)
  expect_true(entry$Package == "breakfast")
  expect_true(entry$Version == "1.0.0")

})

test_that("available_packages() succeeds with unnamed repositories", {
  skip_on_cran()
  renv_tests_scope()

  entry <- renv_available_packages_entry(
    package = "breakfast",
    type    = "source",
    filter  = "1.0.0"
  )

  expect_identical(entry$Package, "breakfast")
  expect_identical(entry$Version, "1.0.0")

})

test_that("renv_available_packages_latest() respects pkgType option", {

  skip_on_cran()
  skip_if(.Platform$pkgType == "source")

  renv_tests_scope()

  renv_scope_options(pkgType = "source")
  record <- renv_available_packages_latest("breakfast")
  expect_identical(attr(record, "type"), "source")

  renv_scope_options(renv.config.ppm.enabled = FALSE)
  renv_scope_options(pkgType = "binary")
  record <- renv_available_packages_latest("breakfast")
  expect_identical(attr(record, "type"), "binary")

})

test_that("pkgType = 'both' on source-only platforms yields a source record (#2275)", {

  skip_on_cran()
  skip_if_not(.Platform$pkgType == "source")

  renv_tests_scope()

  renv_scope_options(pkgType = "both")
  record <- renv_available_packages_latest("breakfast")
  expect_identical(attr(record, "type"), "source")

})

test_that("local sources are preferred when available", {

  skip_on_cran()
  renv_tests_scope()

  renv_scope_envvars(RENV_PATHS_LOCAL = renv_tests_path("local"))

  record <- renv_available_packages_latest(package = "skeleton", type = "source")
  expect_identical(record$Source, "Cellar")

})

test_that("available packages database refreshed on http_proxy change", {

  skip_on_cran()
  skip_on_os("windows")

  renv_tests_scope_repos()
  renv_scope_envvars("https_proxy" = "123")
  available_packages(type = "source")

  count <- 0L
  renv_scope_trace(
    what   = renv:::renv_available_packages_query,
    tracer = function() { count <<- count + 1L }
  )

  renv_scope_envvars("https_proxy" = "")
  available_packages(type = "source")
  expect_identical(count, 1L)

})

test_that("available packages prefer tagged repository", {

  skip_on_cran()
  skip_on_os("windows")

  renv_tests_scope()

  repos <- getOption("repos")[[1L]]
  renv_scope_options(repos = c(CRAN = repos, ALT = repos))

  entry <- renv_available_packages_entry(
    package = "breakfast",
    type    = "source",
    prefer  = "ALT",
    quiet   = TRUE
  )

  expect_equal(entry$Name, "ALT")

})

test_that("we're compatible with R", {

  skip_on_cran()
  renv_tests_scope()
  repos <- getOption("repos")[1L]

  lhs <- as.data.frame(
    available.packages(
      type = "source",
      repos = repos,
      filters = c("R_version", "OS_type")
    ),
    row.names = FALSE,
    stringsAsFactors = FALSE
  )

  rhs <- available_packages(
    type = "source",
    repos = repos
  )[[1L]]

  row.names(lhs) <- row.names(rhs) <- NULL
  fields <- c("Package", "Version")
  expect_equal(lhs[fields], rhs[fields])

})

test_that("we can query the R universe", {
  skip_on_cran()

  lhs <- as.data.frame(
    available.packages(
      type = "source",
      repos = "https://rstudio.r-universe.dev"
    ),
    stringsAsFactors = FALSE
  )

  rhs <- available_packages(
    type = "source",
    repos = "https://rstudio.r-universe.dev/"
  )[[1L]]

  # skip renv, since we just updated it
  lhs <- lhs[lhs$Package != "renv", ]
  rhs <- rhs[rhs$Package != "renv", ]

  # reduce risk of false positive test failures
  rownames(lhs) <- rownames(rhs) <- NULL
  lhs$MD5sum <- rhs$MD5sum <- NULL
  lhs$Remotes <- rhs$Remotes <- NULL

  # R-devel may add new columns to available.packages();
  # restrict comparison to the columns renv produces
  lhs <- lhs[intersect(names(lhs), names(rhs))]

  # otherwise, check they're identical
  expect_identical(lhs, rhs)

})

test_that("available_packages() tolerates missing repositories", {
  renv_tests_scope()

  repos <- getOption("repos")
  repos[["NARC"]] <- file.path(repos[["CRAN"]], "missing")
  renv_scope_options(repos = repos)

  dbs <- available_packages(type = "source")
  expect_false(is.null(dbs[["CRAN"]]))
  expect_true(is.null(dbs[["NARC"]]))
})

test_that("a failed repository query is not cached in the index (#2350)", {

  renv_tests_scope()

  # add a local repository which doesn't exist yet
  repopath <- renv_scope_tempfile("renv-repos-")
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"

  repos <- getOption("repos")
  repos[["LOCAL"]] <- sprintf(fmt, repopath)
  renv_scope_options(repos = repos)

  dbs <- available_packages(type = "source")
  expect_false(is.null(dbs[["CRAN"]]))
  expect_true(is.null(dbs[["LOCAL"]]))

  # now, create the repository
  contrib <- file.path(repopath, "src/contrib")
  ensure_directory(contrib)
  file.create(file.path(contrib, "PACKAGES"))

  # clear the in-memory memoization, so we hit the on-disk index
  renv_dynamic_reset()

  # the repository should now be visible; a partial result
  # from the previous query should not have been cached
  dbs <- available_packages(type = "source")
  expect_false(is.null(dbs[["CRAN"]]))
  expect_false(is.null(dbs[["LOCAL"]]))

})

test_that("failed archive queries are not cached in the index (#2350)", {

  renv_tests_scope()

  # a local repository which doesn't exist yet
  repopath <- renv_scope_tempfile("renv-repos-")
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  repo <- sprintf(fmt, repopath)

  count <- 0L
  renv_scope_trace(
    what   = renv:::renv_available_packages_latest_archive_query_impl,
    tracer = function() count <<- count + 1L
  )

  # repeated lookups within a single operation only query once
  repos <- c(CRAN = repo, EXTRA = repo)
  entry <- renv_available_packages_latest_archive("bread", repos = repos)
  expect_null(entry)
  expect_identical(count, 1L)

  # however, the failure is not cached in the on-disk index,
  # so a new operation re-queries the repository
  renv_dynamic_reset()
  entry <- renv_available_packages_latest_archive("bread", repos = repos)
  expect_null(entry)
  expect_identical(count, 2L)

  # create the archive metadata, and check the query now succeeds
  meta <- file.path(repopath, "src/contrib/Meta")
  ensure_directory(meta)
  database <- list(bread = data.frame(size = 1024L, row.names = "bread/bread_0.5.0.tar.gz"))
  saveRDS(database, file = file.path(meta, "archive.rds"))

  renv_dynamic_reset()
  entry <- renv_available_packages_latest_archive("bread", repos = repos)
  expect_identical(entry$Package, "bread")
  expect_identical(entry$Version, "0.5.0")
  expect_identical(count, 3L)

  # successful queries are cached in the index, as before
  renv_dynamic_reset()
  entry <- renv_available_packages_latest_archive("bread", repos = repos)
  expect_identical(entry$Package, "bread")
  expect_identical(count, 3L)

})

test_that("crandb query returns R-compatible versions", {
  skip_on_cran()
  skip_if_offline()
  skip_if_offline(host = "r-pkg.org")
  renv_scope_options(renv.config.crandb.enabled = TRUE)

  # Query crandb for a package with known R version requirements
  json <- renv_available_packages_crandb_query("forcats")
  expect_type(json, "list")
  expect_true("versions" %in% names(json))

  # Test R compatibility checking
  # forcats 1.0.1 requires R >= 4.1
  entry_101 <- json$versions[["1.0.1"]]
  expect_true(renv_available_packages_crandb_r_compatible(entry_101, "4.5.0"))
  expect_true(renv_available_packages_crandb_r_compatible(entry_101, "4.1.0"))
  expect_false(renv_available_packages_crandb_r_compatible(entry_101, "4.0.0"))

  # forcats 1.0.0 requires R >= 3.4
  entry_100 <- json$versions[["1.0.0"]]
  expect_true(renv_available_packages_crandb_r_compatible(entry_100, "4.0.0"))
  expect_true(renv_available_packages_crandb_r_compatible(entry_100, "3.4.0"))
  expect_false(renv_available_packages_crandb_r_compatible(entry_100, "3.3.0"))

})

test_that("crandb returns newest compatible version", {
  skip_on_cran()
  skip_if_offline()
  skip_if_offline(host = "r-pkg.org")
  renv_scope_options(renv.config.crandb.enabled = TRUE)

  # Test that we get the newest compatible version
  result <- renv_available_packages_latest_crandb("forcats")
  expect_type(result, "list")
  expect_equal(result$Package, "forcats")
  expect_equal(result$Source, "Repository")
  expect_equal(result$Repository, "CRAN")

  # Version should be valid

  expect_true(nzchar(result$Version))
  expect_no_error(numeric_version(result$Version))
})

test_that("when crandb is enabled, repos entry is preferred when versions match (tagged record)", {
  skip_on_cran()
  skip_if_offline()
  skip_if_offline(host = "r-pkg.org")

  # Use real CRAN so both repos and crandb can return the same package version
  renv_scope_options(
    repos = c(CRAN = "https://cloud.r-project.org"),
    renv.config.crandb.enabled = TRUE,
    pkgType = "source"
  )

  # For a package live on CRAN, both repos and crandb return the same version.
  # We must get the repos (tagged) entry so retrieval uses the current URL,
  # not the archive (#1735).
  record <- renv_available_packages_latest("RColorBrewer", type = "source")
  expect_true(renv_record_tagged(record), info = "record should be from repos (tagged), not crandb (untagged)")
  expect_equal(record$Package, "RColorBrewer")
  expect_true(nzchar(record$Version))
})

test_that("a repository record is preferred over a newer crandb version", {

  renv_tests_scope()
  renv_scope_options(renv.config.crandb.enabled = TRUE)

  # crandb isn't restricted to the configured repositories, so it can name a
  # version those repositories can't supply -- as happens when they're pinned
  # to a dated snapshot. the repository record has to win, or renv resolves to
  # a version it can't install from the repositories it was given (#2345)
  local_mocked_bindings(
    renv_available_packages_latest_crandb = function(package, ...) {
      list(
        Package    = package,
        Version    = "9.9.9",
        Source     = "Repository",
        Repository = "CRAN"
      )
    }
  )

  record <- renv_available_packages_latest("bread")

  expect_equal(record$Version, "1.0.0")
  expect_true(renv_record_tagged(record))

})

test_that("crandb is still used when the repositories have no candidate", {

  renv_tests_scope()
  renv_scope_options(renv.config.crandb.enabled = TRUE)

  # this is what crandb is for: naming a version renv can go find when the
  # configured repositories can't supply the package at all
  local_mocked_bindings(
    renv_available_packages_latest_crandb = function(package, ...) {
      list(
        Package    = package,
        Version    = "9.9.9",
        Source     = "Repository",
        Repository = "CRAN"
      )
    }
  )

  record <- renv_available_packages_latest("nonexistent.package")

  expect_equal(record$Version, "9.9.9")
  expect_false(renv_record_tagged(record))

})

test_that("the archive is consulted when the repositories have no candidate", {

  renv_tests_scope()
  renv_scope_options(
    renv.config.crandb.enabled = FALSE,
    renv.install.allowArchivedPackages = TRUE
  )

  # a repository which serves nothing from PACKAGES, but does archive 'gravy'
  archive <- renv_tests_archive_repo()
  renv_scope_options(repos = c(ARCHIVED = archive$url))

  # the archive method was added in #1771 and lost its slot in #2215, when
  # crandb was inserted ahead of it and the picker kept reading only the first
  # two entries -- so an archived-only package resolved to nothing at all
  record <- renv_available_packages_latest("gravy")

  expect_equal(record$Version, "0.5.0")
  expect_equal(record$Repository, "ARCHIVED")

})

test_that("an archived record carries a usable download URL", {

  renv_tests_scope()
  renv_scope_options(
    renv.config.crandb.enabled = FALSE,
    renv.install.allowArchivedPackages = TRUE
  )

  archive <- renv_tests_archive_repo()
  renv_scope_options(repos = c(ARCHIVED = archive$url))

  record <- renv_available_packages_latest("gravy")

  # archives only ever hold source tarballs
  expect_true(renv_record_tagged(record))
  expect_equal(attr(record, "type", exact = TRUE), "source")

  # the record has to survive URL construction: an untagged record yields a
  # zero-length url, which aborts the entire parallel download batch rather
  # than just this package
  info <- renv_graph_url_repository_record(record, record)
  expect_equal(basename(info$url), "gravy_0.5.0.tar.gz")
  expect_equal(basename(dirname(info$url)), "gravy")

})

test_that("an archived record keeps its Repository with unnamed repositories", {

  renv_tests_scope()

  archive <- renv_tests_archive_repo()
  repo <- archive$url

  # renv supports unnamed repositories; names(repos)[[i]] is NULL for these,
  # and assigning that to entry$Repository used to delete the field outright
  entry <- renv_available_packages_latest_archive("gravy", repos = repo)

  expect_equal(entry$Repository, repo)

})

test_that("an archive does not satisfy a binary-only request", {

  renv_tests_scope()
  archive <- renv_tests_archive_repo()
  renv_scope_options(
    repos = c(ARCHIVED = archive$url),
    renv.config.crandb.enabled = FALSE,
    renv.install.allowArchivedPackages = TRUE
  )

  entry <- renv_available_packages_latest_archive(
    package = "gravy",
    type = "binary"
  )

  expect_null(entry)
  expect_equal(
    renv_available_packages_latest_archive("gravy", type = "both")$Version,
    "0.5.0"
  )
  expect_null(renv_available_packages_latest_archive("gravy", type = "mac.binary"))

  renv_scope_options(pkgType = NULL)
  expect_equal(
    renv_available_packages_latest_archive("gravy")$Version,
    "0.5.0"
  )

  expect_error(
    suppressWarnings(renv_available_packages_latest("gravy", type = "binary")),
    "package 'gravy' is not available"
  )

})

test_that("the repositories short-circuit the later lookup methods", {

  renv_tests_scope()
  renv_scope_options(
    renv.config.crandb.enabled = FALSE,
    renv.install.allowArchivedPackages = TRUE
  )

  # 'bread' is live in the test repository, so nothing after it should be
  # consulted at all -- these methods reach the network
  count <- 0L
  renv_scope_trace(
    what   = renv:::renv_available_packages_latest_archive,
    tracer = function() count <<- count + 1L
  )

  record <- renv_available_packages_latest("bread")

  expect_equal(record$Version, "1.0.0")
  expect_identical(count, 0L)

})

test_that("P3M is not consulted when picking a latest version (#1901)", {

  renv_tests_scope()
  renv_scope_options(renv.config.crandb.enabled = FALSE)

  # P3M ignores the configured repositories entirely, so letting it name a
  # version means renv can prefer a P3M binary over the pinned repository
  # snapshot the user actually asked for
  # force the conditions under which P3M would be eligible, so this actually
  # asserts something on platforms where the test scope leaves it disabled
  enabled_called <- FALSE
  latest_called <- FALSE
  local_mocked_bindings(
    renv_p3m_enabled = function() {
      enabled_called <<- TRUE
      TRUE
    },
    renv_available_packages_latest_p3m = function(package, ...) {
      latest_called <<- TRUE
      list(
        Package    = package,
        Version    = "9.9.9",
        Source     = "Repository",
        Repository = "P3M"
      )
    }
  )

  record <- renv_available_packages_latest("bread")

  expect_false(enabled_called)
  expect_false(latest_called)
  expect_equal(record$Version, "1.0.0")

})

test_that("version requirement parsing works correctly", {

  # Test various requirement formats
  expect_true(renv_version_requirement_satisfied("4.0.0", ">= 3.4"))
  expect_true(renv_version_requirement_satisfied("4.0.0", ">= 4.0"))
  expect_true(renv_version_requirement_satisfied("4.0.0", ">= 4.0.0"))
  expect_false(renv_version_requirement_satisfied("4.0.0", ">= 4.1"))

  expect_true(renv_version_requirement_satisfied("3.6.0", "> 3.5"))
  expect_false(renv_version_requirement_satisfied("3.5.0", "> 3.5"))

  expect_true(renv_version_requirement_satisfied("4.0.0", "<= 4.0"))
  expect_false(renv_version_requirement_satisfied("4.1.0", "<= 4.0"))

  # Empty/null requirements should be satisfied
  expect_true(renv_version_requirement_satisfied("4.0.0", NULL))
  expect_true(renv_version_requirement_satisfied("4.0.0", "*"))
  expect_true(renv_version_requirement_satisfied("4.0.0", ""))

})
