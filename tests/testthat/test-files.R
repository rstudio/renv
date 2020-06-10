
context("Files")

test_that("directories can be copied", {

  source <- renv_tempfile("renv-source-")
  target <- renv_tempfile("renv-target-")

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

  source <- renv_tempfile("renv-source-")
  target <- renv_tempfile("renv-target-")

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

test_that("attempts to link files work", {

  source <- renv_tempfile("renv-source-")
  target <- renv_tempfile("renv-target-")

  dir.create(source)
  renv_file_link(source, target)
  expect_true(renv_file_same(source, target))

})

test_that("scoped backups are cleared as necessary", {

  source <- renv_tempfile("renv-source-")
  target <- renv_tempfile("renv-target-")

  writeLines("source", source)
  writeLines("target", target)

  local({
    callback <- renv_file_backup(target)
    on.exit(callback(), add = TRUE)
    expect_true(!file.exists(target))
  })

  expect_true(file.exists(target))
  expect_equal(readLines(target), "target")

  local({
    callback <- renv_file_backup(target)
    on.exit(callback(), add = TRUE)
    writeLines("mutate", target)
  })

  expect_true(file.exists(target))
  expect_equal(readLines(target), "mutate")
  list.files(tempdir())

  files <- list.files(tempdir())
  backup <- grep("^\\.renv-backup-", files)
  expect_length(backup, 0)

})

test_that("renv tempfiles are deleted at end of scope", {

  path <- NULL
  path2 <- NULL
  local({
    path <<- renv_tempfile()
    path2 <<- renv_tempfile()
    file.create(path, path2)
    expect_true(file.exists(path))
    expect_true(file.exists(path2))
  })
  expect_false(file.exists(path))
  expect_false(file.exists(path2))

})

test_that("renv_file_find finds parent files", {

  base <- tempfile("renv-files-")
  rest <- c("alpha/beta/gamma")
  tip <- file.path(base, rest)
  ensure_directory(tip)

  found <- renv_file_find(tip, function(path) {
    if (basename(path) == "alpha")
      return(path)
  })

  expect_true(renv_file_same(found, file.path(base, "alpha")))

})

test_that("attempts to overwrite existing files are handled appropriately", {

  source <- renv_tempfile("renv-source-")
  target <- renv_tempfile("renv-target-")

  writeLines("alpha", con = source)
  writeLines("beta",  con = target)

  expect_error(renv_file_copy(source, target))
  expect_true(renv_file_copy(source, target, overwrite = TRUE))

})

test_that("permissions, timestamps are preserved", {

  source <- renv_tempfile("renv-source-")
  target <- renv_tempfile("renv-target-")

  ensure_directory(source)

  range <- 1:10
  files <- sprintf("%02i.txt", range)
  for (i in range) {
    Sys.sleep(0.01)
    file.create(file.path(source, sprintf("%02i.txt", i)))
  }

  renv_file_copy(source, target)

  srcfiles <- list.files(source, full.names = TRUE)
  tgtfiles <- list.files(target, full.names = TRUE)

  srcinfo <- file.info(srcfiles)
  tgtinfo <- file.info(tgtfiles)

  rownames(srcinfo) <- rownames(tgtinfo) <- basename(srcfiles)

  fields <- setdiff(names(srcinfo), c("ctime", "atime"))
  expect_equal(srcinfo[fields], tgtinfo[fields])

})

test_that("renv can list files not representable in the native encoding", {

  renv_scope_tempdir()
  evil <- "\u9b3c"

  file.create(evil)
  on.exit(unlink(evil), add = TRUE)

  files <- renv_file_list(getwd(), full.names = FALSE)
  expect_true(evil %in% files)

})
