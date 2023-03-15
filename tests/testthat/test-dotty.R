
context("Dotty")

registerS3method("[<-", "__renv_dotty__", dotty, envir = renv_envir_self())

test_that("index-based destructuring works", {

  .[value] <- list(1L)
  expect_equal(value, 1L)

  .[nr, nc] <- dim(mtcars)
  expect_equal(nr, 32)
  expect_equal(nc, 11)

})

test_that("name-based destructuring works", {

  .[a = x, b = y] <- list(y = "y", x = "x")
  expect_equal(a, "x")
  expect_equal(b, "y")

})

test_that("we can use dotdot to drop unneeded values", {

  .[a, .., z] <- letters
  expect_equal(a, 'a')
  expect_equal(z, 'z')

})

test_that("we can capture a single value by name", {

  .[x = mpg] <- mtcars
  expect_equal(x, mtcars$mpg)

})

test_that("we can use a leading '..' to drop leading values", {

  .[.., z] <- letters
  expect_equal(z, 'z')

  .[.., y, z] <- letters
  expect_equal(y, 'y')
  expect_equal(z, 'z')

})

test_that("we can use a trailing '..' to drop trailing values", {

  .[a, ..] <- letters
  expect_equal(a, 'a')

  .[a, b, ..] <- letters
  expect_equal(a, 'a')
  expect_equal(b, 'b')

})

test_that("we can support arbitrary expressions", {

  .[a = mpg * 2] <- mtcars
  expect_equal(a, mtcars$mpg * 2)

  .[mpg_cyl = mpg * cyl] <- mtcars
  expect_equal(mpg_cyl, mtcars$mpg * mtcars$cyl)

})

test_that("we support recursive invocations", {

  .[a, .[b, .[c]]] <- list(1, list(2, list(3)))

  expect_equal(a, 1)
  expect_equal(b, 2)
  expect_equal(c, 3)

})
