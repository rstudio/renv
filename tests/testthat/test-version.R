
test_that("various versions can be compared", {

  expect_equal(renv_version_compare("3.5",   "3.5.7"), -1L)
  expect_equal(renv_version_compare("3.5.0", "3.5.7"), -1L)
  expect_equal(renv_version_compare("3.5.7", "3.5.7"), +0L)
  expect_equal(renv_version_compare("3.5.8", "3.5.7"), +1L)
  expect_equal(renv_version_compare("3.10.1", "4"), -1L)

})

test_that("version matching works as expected", {

  versions <- c("2.7.10", "3.5.1", "3.5.10", "3.6", "3.6.5")

  expect_equal(renv_version_match(versions, "3.5.6"), "3.5.10")
  expect_equal(renv_version_match(versions, "3.5.1"), "3.5.1")
  expect_equal(renv_version_match(versions, "3"), "3.6.5")
  expect_equal(renv_version_match(versions, "2"), "2.7.10")

})

test_that("renv_version_length works as expected", {
  expect_equal(renv_version_length("1"), 1)
  expect_equal(renv_version_length("1.2"), 2)
  expect_equal(renv_version_length("1.2-3"), 3)
  expect_equal(renv_version_length("1.2.3-4"), 4)
})

test_that("renv_version_parts works as expected", {
  expect_equal(renv_version_parts("1.0", 1L), c(1L))
  expect_equal(renv_version_parts("1.0", 2L), c(1L, 0L))
  expect_equal(renv_version_parts("1.0", 3L), c(1L, 0L, 0L))
  expect_equal(renv_version_parts("1.1-4", 3L), c(1L, 1L, 4L))
})

test_that("renv_version_rank produces the same ordering as numeric_version", {

  packages <- c("A", "A", "B", "B", "A", "C", "B", "C")
  versions <- c("1.2", "1.2.1", "1.4-5.100", "1.4-5.99",
                 "1.4", "2.0", "1.10.0", "1.9.0")

  rank <- renv_version_rank(versions)

  expected <- order(packages, numeric_version(versions), decreasing = TRUE)
  actual <- order(packages, rank, decreasing = TRUE)
  expect_identical(actual, expected)

  expected <- order(packages, numeric_version(versions), decreasing = FALSE)
  actual <- order(packages, rank, decreasing = FALSE)
  expect_identical(actual, expected)

})

test_that("renv_version_rank handles single-component versions", {

  versions <- c("3", "1", "2")

  expected <- order(numeric_version(versions))
  actual <- order(renv_version_rank(versions))
  expect_identical(actual, expected)

})

test_that("renv_version_rank handles mixed-length version components", {

  versions <- c("1.0", "1.0.0", "1.0.0.0", "1.0.1", "0.99.9")

  expected <- order(numeric_version(versions))
  actual <- order(renv_version_rank(versions))
  expect_identical(actual, expected)

})
