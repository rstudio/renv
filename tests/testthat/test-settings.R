
test_that("renv.settings can be used to provide defaults", {

  renv_tests_scope()
  expect_equal(settings$snapshot.type(), "implicit")

  # project is not yet initialized, so defaults can be used
  local({
    renv_scope_options(renv.settings.snapshot.type = "all")
    expect_equal(settings$snapshot.type(), "all")
  })

  local({
    renv_scope_options(renv.settings = list(snapshot.type = "all"))
    expect_equal(settings$snapshot.type(), "all")
  })

})

test_that("non-persistent settings exist in R session; not in file", {

  renv_tests_scope()
  expect_equal(settings$snapshot.type(), "implicit")

  project <- getwd()
  path <- "renv/settings.json"
  before <- renv_settings_read_impl(path)
  settings$snapshot.type("all", persist = FALSE)
  after <- renv_settings_read_impl(path)

  expect_equal(before, after)
  expect_equal(settings$snapshot.type(), "all")

  settings$ignored.packages("dplyr", persist = TRUE)

  settings <- renv_settings_get(project)
  persisted <- renv_settings_read_impl(path)

  # TODO
  settings$ppm.ignored.urls <- persisted$ppm.ignored.urls <- NULL

  expect_mapequal(settings, persisted)

})

test_that("users can request specific versions of R for lockfile", {

  renv_tests_scope()
  renv_scope_options(renv.settings.r.version = "4.0")

  init()

  lockfile <- renv_lockfile_load(getwd())
  expect_identical(lockfile$R$Version, "4.0")

})

test_that("project settings are migrated from dcf to json", {

  project <- renv_tests_scope()
  init()

  settings <- heredoc("
    bioconductor.version: 3.16
    external.libraries:
    ignored.packages:
    package.dependency.fields: Imports, Depends, LinkingTo
    r.version:
    snapshot.type: implicit
    use.cache: TRUE
    vcs.ignore.cellar: TRUE
    vcs.ignore.library: TRUE
    vcs.ignore.local: TRUE
  ")

  writeLines(settings, con = "renv/settings.dcf")
  old <- renv_settings_read(file.path(project, "renv/settings.dcf"))
  unlink("renv/settings.json")

  renv_settings_migrate(project)
  expect_true(file.exists("renv/settings.json"))
  new <- renv_settings_read(file.path(project, "renv/settings.json"))

  # TODO
  old$ppm.ignored.urls <- new$ppm.ignored.urls <- NULL

  expect_mapequal(old, new)

})

test_that("a settings global option is handled correctly", {
  renv_tests_scope()
  renv_scope_options(renv.settings = list(ignored.packages = "odbc"))
  expect_equal(settings$ignored.packages(), "odbc")
  expect_null(settings$bioconductor.version())
})
