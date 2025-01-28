
test_that("jobs can be run", {
  skip_on_cran()
  encoded <- job(function() { renv_base64_encode("hello") })
  expect_equal(encoded, "aGVsbG8=")
})
