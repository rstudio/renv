
test_that("we compute identical hashes for old + new md5 implementations", {

  skip_if(getRversion() < "4.5.0")

  examples <- c("Hello, world!", "a\nb\nc\n", "d\r\ne\r\nf\r\n")
  for (example in examples) {
    old <- renv_md5sum_old(example)
    new <- renv_md5sum_new(example)
    expect_equal(old, new)
  }

})
