
context("Repositories")

test_that("we can query our local repository during tests", {
  ap <- available_packages(type = "source")
  expect_setequal(ap$Package, c("bread", "breakfast", "egg", "oatmeal", "toast"))
})

