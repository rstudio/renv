
test_that("progress totals can grow as work is discovered", {

  tick <- renv_progress_create(max = 1L, wait = 0)
  output <- capture.output(tick(), tick(3L))

  expect_match(output, "[1/1]", fixed = TRUE)
  expect_match(output, "[2/3]", fixed = TRUE)

})
