context("Manifest")

test_that("manifests can be read from file", {

  text <- '
[Section1]
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
