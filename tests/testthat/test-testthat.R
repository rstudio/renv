
# the condition handler in our test_that wrapper should prevent
# this from being a real error; it should just be a skip
test_that("errors can be caught and skipped", {
  stop("download failed [error code 22]")
})
