
test_that("renv::embed() works with .R files", {

  project <- renv_tests_scope("breakfast")
  install("breakfast")

  writeLines("library(breakfast)", con = "test-embed.R")
  embed("test-embed.R")
  expect_snapshot(writeLines(readLines("test-embed.R")))

})

test_that("renv::embed() works with .Rmd files", {

  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  project <- renv_tests_scope("breakfast", libpaths = TRUE)
  install("breakfast")

  contents <- heredoc('
    ---
    title: Embedding Test
    ---

    ```{r}
    library(breakfast)
    ```
  ')

  writeLines(contents, con = "test-embed.Rmd")
  withCallingHandlers(
    embed("test-embed.Rmd"),
    warning = function(cnd) invokeRestart("muffleWarning")
  )

  contents <- readLines("test-embed.Rmd")
  expect_true(any(grepl("bread@1.0.0", contents)))
  expect_true(any(grepl("oatmeal@1.0.0", contents)))
  expect_true(any(grepl("toast@1.0.0", contents)))

})

test_that("missing packages are reported", {

  project <- renv_tests_scope("breakfast")

  writeLines("library(breakfast)", con = "test-embed.R")
  expect_error(embed("test-embed.R"))

})

test_that("embed(lockfile = FALSE) ignores the lockfile", {

  project <- renv_tests_scope("bread")
  init()

  install("breakfast")
  writeLines("library(breakfast)", con = "test-embed.R")
  embed("test-embed.R", lockfile = FALSE)
})

test_that("embed warns when lockfile is missing dependencies", {

  project <- renv_tests_scope("bread")
  init()

  # the lockfile only has 'bread', but the script needs 'breakfast'
  install("breakfast")
  writeLines("library(breakfast)", con = "test-embed.R")

  expect_condition(
    embed("test-embed.R"),
    class = "renv.embed.missing_packages"
  )

})

test_that("embed(lockfile = NA) uses repository package versions", {

  project <- renv_tests_scope("breakfast")
  install("breakfast")

  writeLines("library(breakfast)", con = "test-embed.R")
  embed("test-embed.R", lockfile = NA)

  contents <- readLines("test-embed.R")
  expect_true(any(grepl("breakfast@1.0.0", contents)))
  expect_true(any(grepl("bread@1.0.0", contents)))
  expect_true(any(grepl("oatmeal@1.0.0", contents)))
  expect_true(any(grepl("toast@1.0.0", contents)))

})

test_that("embed(lockfile = NA) works even when packages are not installed", {

  project <- renv_tests_scope()

  # script references 'bread' which is NOT installed,
  # but is available in the test repository
  writeLines("library(bread)", con = "test-embed.R")
  embed("test-embed.R", lockfile = NA)

  contents <- readLines("test-embed.R")
  expect_true(any(grepl("bread@1.0.0", contents)))

})
