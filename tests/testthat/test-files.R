context("Files")

test_that("directories can be copied", {

  source <- tempfile("renv-source-")
  target <- tempfile("renv-target-")

  ensure_directory(source)

  range <- 1:10
  files <- sprintf("%02i.txt", range)
  for (i in range)
    file.create(file.path(source, sprintf("%02i.txt", i)))

  expect_setequal(files, list.files(source))
  renv_file_copy(source, target)
  expect_setequal(list.files(source), list.files(target))

})

test_that("directories can be moved", {

  source <- tempfile("renv-source-")
  target <- tempfile("renv-target-")

  ensure_directory(source)

  range <- 1:10
  files <- sprintf("%02i.txt", range)
  for (i in range)
    file.create(file.path(source, sprintf("%02i.txt", i)))

  renv_file_move(source, target)
  expect_true(!file.exists(source))
  expect_true(file.exists(target))
  expect_setequal(files, list.files(target))

})
