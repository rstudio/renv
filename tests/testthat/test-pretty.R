test_that("renv_pretty_print() creates bulleted list with optional preamble/postable", {
  expect_snapshot({
    renv_pretty_print(letters[1:3])
    renv_pretty_print(letters[1:3], preamble = "before")
    renv_pretty_print(letters[1:3], postamble = "after")
  })
})

test_that("renv_pretty_print() doesn't show pre/post amble if no values", {
  expect_silent(renv_pretty_print(character(), "before", "after"))

})
