context("Manifest")

test_that("manifests can be read from file", {

  text <- '
[Section1]
# This is a comment.
Key1=Value1
Key2=Value2

[Section2]
Key1=Value1

Key2=Value2
'

  file <- tempfile()
  writeLines(text, con = file)
  output <- renv_manifest_read(file)
  expected <- list(

    Section1 = list(
      Key1 = "Value1",
      Key2 = "Value2"
    ),

    Section2 = list(
      Key1 = "Value1",
      Key2 = "Value2"
    )

  )
  expect_identical(output, expected)

})

test_that("manifests can be diffed", {

  lhs <- list(A = 1, B = 2, C = "a", D = list(E = 1, F = 2))
  rhs <- list(A = 1, B = 3, C = "b", D = list(E = 1, F = 3))

  diff <- renv_manifest_diff(lhs, rhs)
  expected <- list(
    B = list(before = 2, after = 3),
    C = list(before = "a", after = "b"),
    D = list(
      F = list(before = 2, after = 3)
    )
  )

  expect_identical(diff, expected)

})
