
test_that("jobs can be run", {
  encoded <- job(function() { renv_base64_encode("hello") })
  expect_equal(encoded, "aGVsbG8=")
})
