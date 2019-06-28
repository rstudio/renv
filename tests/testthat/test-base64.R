
context("Base64")

test_that("some sample strings can be base64-encoded", {
  expect_equal(renv_base64_encode("renv"), "cmVudg==")
})
