
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
  expect_true(!renv_file_exists(source))
  expect_true(renv_file_exists(target))
  expect_setequal(files, list.files(target))

})

test_that("attempts to link files work", {

  source <- tempfile("renv-source-")
  target <- tempfile("renv-target-")

  file.create(source)
  renv_file_link(source, target)
  expect_true(renv_file_same(source, target))

})

test_that("scoped backups are cleared as necessary", {

  source <- tempfile("renv-source-")
  target <- tempfile("renv-target-")

  writeLines("source", source)
  writeLines("target", target)

  local({
    callback <- renv_file_scoped_backup(target)
    on.exit(callback(), add = TRUE)
    expect_true(!renv_file_exists(target))
  })

  expect_true(renv_file_exists(target))
  expect_equal(readLines(target), "target")

  local({
    callback <- renv_file_scoped_backup(target)
    on.exit(callback(), add = TRUE)
    writeLines("mutate", target)
  })

  expect_true(renv_file_exists(target))
  expect_equal(readLines(target), "mutate")
  list.files(tempdir())

  files <- list.files(tempdir())
  backup <- grep("^renv-backup-", files)
  expect_length(backup, 0)

})
