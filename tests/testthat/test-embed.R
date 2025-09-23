
test_that("renv::embed() works with .R files", {

  project <- renv_tests_scope("breakfast")
  install("breakfast")

  writeLines("library(breakfast)", con = "test-embed.R")
  embed("test-embed.R")
  expect_snapshot(writeLines(readLines("test-embed.R")))

})

test_that("renv::embed() works with .Rmd files", {

  project <- renv_tests_scope("breakfast")
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

  expect_snapshot(writeLines(readLines("test-embed.Rmd")))

})

test_that("missing packages are reported", {

  project <- renv_tests_scope("breakfast")

  writeLines("library(breakfast)", con = "test-embed.R")
  expect_error(embed("test-embed.R"))

})
