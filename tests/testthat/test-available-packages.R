
context("Available Packages")

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

test_that("available_packages() errs on incorrect repository", {
  skip_on_cran()

  renv_scope_options(
    renv.config.connect.timeout = 1L,
    renv.config.connect.retry   = 0L,
    repos = c(CRAN = "https://www.example.com/no/such/repository")
  )

  expect_error(available_packages(type = "source"))
})

test_that("renv handles multiple available source packages", {
  skip_on_cran()

  renv_scope_options(repos = getOption("repos"))
  renv_tests_scope()
  repos <- tempfile("renv-test-repos-")
  renv_tests_init_repos(repos)

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
  renv_scope_options(repos = unname(getOption("repos")))

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

  # NOTE: this fails because we don't populate binary repositories during tests
  renv_scope_options(renv.config.mran.enabled = FALSE)
  renv_scope_options(pkgType = "binary")
  expect_error(renv_available_packages_latest("breakfast"))

})

test_that("local sources are preferred when available", {

  skip_on_cran()
  renv_tests_scope()

  root <- renv_tests_root()
  renv_scope_envvars(RENV_PATHS_LOCAL = file.path(root, "local"))

  record <- renv_available_packages_latest(package = "skeleton", type = "source")
  expect_identical(record$Source, "Cellar")

})

test_that("available packages database refreshed on http_proxy change", {

  skip_on_cran()
  skip_on_os("windows")

  count <- 0L
  renv_scope_trace(
    what   = renv:::renv_available_packages_query,
    tracer = function() { count <<- count + 1L }
  )

  Sys.setenv("https_proxy" = "")
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
  skip_sometimes()

  lhs <- as.data.frame(
    available.packages(
      type = "source",
      repos = "https://rstudio.r-universe.dev"
    )
  )

  rhs <- available_packages(
    type = "source",
    repos = "https://rstudio.r-universe.dev/"
  )[[1L]]

  rownames(lhs) <- rownames(rhs) <- NULL
  expect_identical(lhs, rhs)

})
