
context("Base64")

test_that("some sample strings can be base64-encoded", {

  expect_equal(renv_base64_encode("renv"), "cmVudg==")

  original <- rawToChar(as.raw(1:255))
  encoded <- renv_base64_encode(original)
  decoded <- renv_base64_decode(encoded)
  expect_equal(original, decoded)

})

test_that("some random base64 strings can be round-tripped", {

  set.seed(123)
  bytes <- 1:255

  text <- replicate(1000, {
    n <- sample(128L, 1L)
    rawToChar(as.raw(sample(bytes, n, replace = TRUE)))
  })

  encoded <- lapply(text, renv_base64_encode)
  decoded <- lapply(encoded, renv_base64_decode)

  expect_true(all(text == decoded))

})
