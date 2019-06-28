
context("Repositories")

test_that("we can query our local repository during tests", {
  ap <- renv_available_packages(type = "source")[[1]]
  expected <- c("bread", "breakfast", "brunch", "egg", "packrat", "oatmeal", "toast")
  expect_setequal(ap$Package, expected)
})
