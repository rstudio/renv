
context("DCF")

test_that("we can read different types of DCFs", {

  expected <- list(A = "1", B = "2")

  # plain jane
  actual <- renv_dcf_read(text = "A: 1\nB: 2")
  expect_equal(actual, expected)

  # extra whitespace between fields
  actual <- renv_dcf_read(text = "A: 1\n\nB: 2\n")
  expect_equal(actual, expected)

})

test_that("we allow for unindented continuations", {

  actual <- renv_dcf_read(text = "A: This field\nisn't indented.\nB: 42")
  expected <- list(A = "This field isn't indented.", B = "42")
  expect_equal(actual, expected)

})

test_that("we can read a latin-1 DESCRIPTION file", {

  # declared latin1; is latin1
  contents <- heredoc({'
    Encoding: latin1
    Dessert: crème brûlée
  '})

  latin1 <- iconv(enc2utf8(contents), from = "UTF-8", to = "latin1")
  file <- tempfile("DESCRIPTION-")
  writeLines(latin1, con = file, useBytes = TRUE)

  dcf <- renv_dcf_read(file)
  expect_equal(dcf$Dessert, "crème brûlée")

})

test_that("we can read a custom encoded DESCRIPTION file", {

  skip_if(!"CP936" %in% iconvlist())

  nihao <- enc2utf8("\u4f60\u597d")

  # declared CP936, is CP936
  contents <- heredoc({'
    Encoding: CP936
    Greeting: \u4f60\u597d
  '})

  bytes <- iconv(
    x = enc2utf8(contents),
    from = "UTF-8",
    to = "CP936",
    toRaw = TRUE
  )

  file <- tempfile("DESCRIPTION-")
  writeBin(bytes[[1L]], con = file)

  dcf <- renv_dcf_read(file)
  expect_equal(dcf$Greeting, nihao)

})

test_that("we can read mis-encoded DESCRIPTION files", {

  # declared UTF-8; but latin1
  contents <- heredoc('
    Encoding: UTF-8
    Dessert: crème brûlée
  ')

  latin1 <- iconv(enc2utf8(contents), from = "UTF-8", to = "latin1")
  file <- tempfile("DESCRIPTION-")
  writeLines(latin1, con = file, useBytes = TRUE)

  dcf <- renv_dcf_read(file)
  expect_equal(dcf$Dessert, "crème brûlée")

})

test_that("we can read and write a dcf file", {

  contents <- heredoc('
    Title: The title.
    Description: The Description field is quite long.
        It needs to wrap across multiple lines.
  ')

  descfile <- renv_scope_tempfile("renv-description-")
  writeLines(contents, con = descfile)

  old <- renv_dcf_read(descfile)
  renv_dcf_write(old, file = descfile)
  new <- read.dcf(descfile, all = TRUE)

  expect_equal(
    gsub("[[:space:]]+", " ", old$Field),
    gsub("[[:space:]]+", " ", new$Field)
  )

})
