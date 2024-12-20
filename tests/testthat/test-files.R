
test_that("directories can be copied", {

  source <- renv_scope_tempfile("renv-source-")
  target <- renv_scope_tempfile("renv-target-")

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

  source <- renv_scope_tempfile("renv-source-")
  target <- renv_scope_tempfile("renv-target-")

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

  source <- renv_scope_tempfile("renv-source-")
  target <- renv_scope_tempfile("renv-target-")

  dir.create(source)
  renv_file_link(source, target)
  expect_true(renv_file_same(source, target))

})

test_that("scoped backups are cleared as necessary", {

  source <- renv_scope_tempfile("renv-source-")
  target <- renv_scope_tempfile("renv-target-")

  writeLines("source", source)
  writeLines("target", target)

  local({
    callback <- renv_file_backup(target)
    defer(callback())
    expect_true(!file.exists(target))
  })

  expect_true(file.exists(target))
  expect_equal(readLines(target), "target")

  local({
    callback <- renv_file_backup(target)
    defer(callback())
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
    path <<- renv_scope_tempfile()
    path2 <<- renv_scope_tempfile()
    file.create(path, path2)
    expect_true(file.exists(path))
    expect_true(file.exists(path2))
  })
  expect_false(file.exists(path))
  expect_false(file.exists(path2))

})

test_that("renv_file_find finds parent files", {

  base <- renv_scope_tempfile("renv-files-")
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

  source <- renv_scope_tempfile("renv-source-")
  target <- renv_scope_tempfile("renv-target-")

  writeLines("alpha", con = source)
  writeLines("beta",  con = target)

  expect_error(renv_file_copy(source, target))
  expect_true(renv_file_copy(source, target, overwrite = TRUE))

})

test_that("permissions, timestamps are preserved", {

  skip("failing test; mtime sometimes differs?")
  source <- renv_scope_tempfile("renv-source-")
  target <- renv_scope_tempfile("renv-target-")

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

  srcinfo <- renv_file_info(srcfiles)
  tgtinfo <- renv_file_info(tgtfiles)

  rownames(srcinfo) <- rownames(tgtinfo) <- basename(srcfiles)

  fields <- setdiff(names(srcinfo), c("ctime", "atime"))
  expect_equal(srcinfo[fields], tgtinfo[fields])

})

test_that("renv can list files not representable in the native encoding", {

  skip_if(renv_platform_unix() && !renv_l10n_utf8())

  renv_scope_tempdir()
  evil <- "\u9b3c"

  file.create(evil)
  defer(unlink(evil))

  files <- renv_file_list(getwd(), full.names = FALSE)
  expect_true(evil %in% files)

})

test_that("renv can detect broken junctions / symlinks", {

  renv_scope_tempdir()

  if (renv_platform_windows()) {

    file.create("file")
    dir.create("dir")
    dir.create("nowhere")
    Sys.junction("dir", "junction")
    Sys.junction("nowhere", "broken")
    unlink("nowhere", recursive = TRUE)

  } else {

    file.create("file")
    dir.create("dir")

    file.symlink("file", "filelink")
    file.symlink("dir", "dirlink")
    file.symlink("oops", "broken")

  }

  paths <- list.files()
  broken <- renv_file_broken(paths)
  expect_equal(paths[broken], "broken")

})

test_that("renv can detect junction points", {
  skip_on_cran()
  skip_if(!renv_platform_windows())

  renv_scope_tempdir()
  dir.create("source")
  defer(unlink("source", recursive = TRUE))

  # create some files -- should give the directory a size,
  # but this seems unreliable?
  files <- sprintf("source/%05i.txt", 1:100)
  file.create(files)

  # create a junction point
  Sys.junction("source", "junction")
  defer(unlink("junction", recursive = TRUE))

  # check that they're the same
  expect_true(renv_file_same("source", "junction"))
})
