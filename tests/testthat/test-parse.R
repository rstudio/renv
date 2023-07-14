
test_that("code containing multibyte characters can be parsed", {

  skip_if(renv_platform_unix() && !renv_l10n_utf8())

  code <- intToUtf8(258L)  # "Ă"
  parsed <- renv_parse_text(code)
  expect_true(inherits(parsed, "expression"))

  code <- intToUtf8(193L)  # "Á"
  parsed <- renv_parse_text(code)
  expect_true(inherits(parsed, "expression"))

  code <- enc2utf8("Á # 鬼")
  parsed <- renv_parse_text(code)
  expect_true(inherits(parsed, "expression"))

  code <- enc2utf8("paste(Á, '鬼')")
  parsed <- renv_parse_text(code)
  expect_true(inherits(parsed, "expression"))

  code <- enc2utf8("paste(`Á`, `鬼`)")
  parsed <- renv_parse_text(code)
  expect_true(inherits(parsed, "expression"))

})
