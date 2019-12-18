
context("Miscellaneous")

# base::sprintf will fail if you pass a format string that's too
# long even if no arguments are used, so renv provides its own
# override that handles this case and then delegates to base::sprintf
# if it's otherwise safe to do so
test_that("long lines and be printed", {
  long <- paste(sample(letters, 8192 * 2, TRUE), collapse = "")
  expect_equal(sprintf(long), long)
})

test_that("unmatched dots cause an error", {
  expect_error(snapshot(packaeg = getwd()))
})
