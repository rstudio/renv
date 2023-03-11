
context("Dynamic")

test_that("dynamic variables are cool and good", {

  a <- 0L
  envir <- environment()

  example <- function() {
    dynamic(
      key   = list(),
      value = a <<- a + 1L,
      envir = envir
    )
  }

  local({

    example()
    expect_equal(a, 1L)

    example()
    expect_equal(a, 1L)

  })


})
