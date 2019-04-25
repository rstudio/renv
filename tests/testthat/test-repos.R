
context("Repositories")

test_that("we can query our local repository during tests", {
  ap <- renv_available_packages(type = "source")[[1]]
  expect_setequal(ap$Package, c("bread", "breakfast", "brunch", "egg", "oatmeal", "toast"))
})

