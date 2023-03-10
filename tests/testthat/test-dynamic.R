
context("Dynamic")

test_that("dynamic variables are cool and good", {

  a <- 0L
  example <- function() {
    dynamic(expr = a <<- a + 1L)
  }

  # NOTE: If running the test interactively, you must run this
  # as an entire block as the value is cleared in a task callback!
  local({

    example()
    expect_true(a == 1L)

    example()
    expected <- if (interactive()) 1L else 2L
    expect_equal(a, expected)

  })


})
