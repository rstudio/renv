
test_that("renv reports errors when decompressing invalid archives", {

  badtar <- renv_scope_tempfile(fileext = ".tar")
  writeLines("oh no", con = badtar)
  expect_error(renv_archive_decompress(badtar, verbose = TRUE))

  badzip <- renv_scope_tempfile(fileext = ".zip")
  writeLines("oh no", con = badzip)
  expect_error(renv_archive_decompress(badzip))

})

test_that("we can successfully compress / decompress some sample files", {

  renv_scope_tempdir()

  for (letter in letters)
    writeLines(letter, con = letter)

  tarfile <- renv_scope_tempfile(fileext = ".tar.gz")
  tar(tarfile, files = ".")

  actual <- list.files()
  expected <- setdiff(basename(renv_archive_list(tarfile)), ".")
  expect_setequal(actual, expected)

  exdir <- renv_scope_tempfile()
  renv_archive_decompress(tarfile, exdir = exdir)
  expect_setequal(list.files(exdir), list.files())

  zipper <- Sys.getenv("R_ZIPCMD", unset = "zip")
  if (nzchar(Sys.which(zipper))) {

    zipfile <- renv_scope_tempfile(fileext = ".zip")
    zip(zipfile, files = ".", extras = "-q")

    actual <- list.files()
    expected <- basename(renv_archive_list(zipfile))
    expect_setequal(actual, expected)

    exdir <- renv_scope_tempfile()
    renv_archive_decompress(zipfile, exdir = exdir)
    expect_setequal(list.files(exdir), list.files())

  }

})

test_that("we can decompress an archive with a tilde path", {
  skip_on_cran()
  skip_on_windows()

  renv_tests_scope()
  renv_scope_envvars(HOME = getwd())
  renv_scope_envvars(tar = Sys.which("tar"))

  # check that we actually set home correctly?
  if (!identical(path.expand("~"), getwd()))
    skip("couldn't override home path")

  # NOTE: in older versions of R, only paths to directory were accepted,
  # so we run out test by attempting to tar up a directory rather than file
  dir.create("subdir")
  writeLines("hello", con = "subdir/a.txt")
  writeLines("goodbye", con = "subdir/b.txt")
  tar("files.tar.gz", files = "subdir", compression = "gzip")
  unlink("subdir", recursive = TRUE)

  # double check the files we have in the archive
  # (renv_archive_list might report the folder itself so test files individually)
  files <- renv_archive_list("files.tar.gz")
  expect_true("subdir/a.txt" %in% files)
  expect_true("subdir/b.txt" %in% files)
  expect_false(file.exists("subdir/a.txt"))
  expect_false(file.exists("subdir/b.txt"))

  archive <- "~/files.tar.gz"
  if (!file.exists(archive))
    skip("archive does not exist in home directory")

  renv_archive_decompress(
    archive = archive,
    exdir   = getwd()
  )

  expect_true(file.exists("subdir/a.txt"))
  expect_true(file.exists("subdir/b.txt"))

})

test_that("tarballs using other compressions are recognized and handled", {

  renv_scope_tempdir()
  for (letter in letters)
    writeLines(letter, con = letter)

  for (compression in c("bzip2", "xz")) {

    ext <- paste0(".tar.", switch(compression, bzip2 = "bz2", xz = "xz"))
    tarfile <- renv_scope_tempfile(fileext = ext)
    tar(tarfile, files = ".", compression = compression)

    expect_equal(renv_archive_type(tarfile), "tar")

    expected <- setdiff(basename(renv_archive_list(tarfile)), ".")
    expect_setequal(list.files(), expected)

    expect_equal(renv_archive_read(tarfile, "./a"), "a")

    exdir <- renv_scope_tempfile()
    renv_archive_decompress(tarfile, exdir = exdir)
    expect_setequal(list.files(exdir), list.files())

  }

  expect_equal(renv_archive_type("pkg_1.0.tar.zst"), "tar")
  expect_equal(renv_archive_type("pkg_1.0.tar.zstd"), "tar")
  expect_equal(renv_archive_type("pkg_1.0.zip"), "zip")
  expect_equal(renv_archive_type("pkg_1.0.rds"), "unknown")

})

test_that("archive type is detected from contents, not just extension", {

  renv_scope_tempdir()
  dir.create("pkg")
  writeLines(c("Package: pkg", "Version: 1.0"), con = "pkg/DESCRIPTION")

  tar("pkg.tgz", files = "pkg", compression = "gzip", tar = "internal")
  tar("pkg.tar", files = "pkg", compression = "none", tar = "internal")
  tar("pkg.tbz", files = "pkg", compression = "bzip2", tar = "internal")
  tar("pkg.txz", files = "pkg", compression = "xz", tar = "internal")

  # tarballs served under extensions renv doesn't recognize
  for (tarball in c("pkg.tgz", "pkg.tar", "pkg.tbz", "pkg.txz")) {
    file.copy(tarball, "pkg.bin", overwrite = TRUE)
    expect_equal(renv_archive_type("pkg.bin"), "tar")
    expect_true("pkg/DESCRIPTION" %in% renv_archive_list("pkg.bin"))
  }

  # a gzip tarball saved under a .zip extension
  file.copy("pkg.tgz", "pkg.zip", overwrite = TRUE)
  expect_equal(renv_archive_type("pkg.zip"), "tar")
  expect_equal(renv_description_read("pkg.zip")$Package, "pkg")

  # a zip archive saved under a tarball extension, and under an unknown one
  zipper <- Sys.getenv("R_ZIPCMD", unset = "zip")
  if (nzchar(Sys.which(zipper))) {

    zip("pkg_1.0.zip", files = "pkg", extras = "-q")
    file.copy("pkg_1.0.zip", "pkg_1.0.tar.xz", overwrite = TRUE)
    file.copy("pkg_1.0.zip", "pkg_1.0.bin", overwrite = TRUE)

    for (archive in c("pkg_1.0.zip", "pkg_1.0.tar.xz", "pkg_1.0.bin")) {
      expect_equal(renv_archive_type(archive), "zip")
      expect_equal(renv_description_read(archive)$Package, "pkg")
      exdir <- renv_scope_tempfile()
      renv_archive_decompress(archive, exdir = exdir)
      expect_true(file.exists(file.path(exdir, "pkg/DESCRIPTION")))
    }

  }

  # unrecognized contents fall back to the extension
  writeLines("not an archive", con = "text.tar.gz")
  expect_equal(renv_archive_type("text.tar.gz"), "tar")
  writeLines("not an archive", con = "text.txt")
  expect_equal(renv_archive_type("text.txt"), "unknown")

  # directories and nonexistent paths use the extension alone
  expect_equal(renv_archive_type("pkg"), "unknown")
  expect_equal(renv_archive_type("missing.tar.gz"), "tar")

})

test_that("compressed files which aren't tarballs aren't treated as archives", {

  renv_scope_tempdir()

  # R's internal tar errors when asked to list a non-tarball, while system
  # tar only warns, so use the internal tar to catch the regression on every
  # platform (#2428)
  renv_scope_envvars(TAR = "internal")

  # PACKAGES.gz and .rds files are gzip / bzip2 / xz streams, but not tarballs
  saveRDS(list(a = 1), "packages.rds")
  saveRDS(list(a = 1), "PACKAGES.rds", compress = "xz")
  saveRDS(list(a = 1), "database.rds", compress = "bzip2")
  con <- gzfile("PACKAGES.gz", open = "w")
  writeLines(c("Package: pkg", "Version: 1.0"), con)
  close(con)

  for (file in c("packages.rds", "PACKAGES.rds", "database.rds", "PACKAGES.gz")) {
    expect_equal(renv_archive_type(file), "unknown")
    expect_false(renv_download_check_archive(file))
  }

  # a compressed non-tarball still falls back to its extension
  file.copy("PACKAGES.gz", "pkg.tar.gz")
  expect_equal(renv_archive_type("pkg.tar.gz"), "tar")

  # downloads of such files aren't rejected as unreadable archives
  url <- paste0("file:", renv_path_normalize("packages.rds"))
  destfile <- renv_scope_tempfile("renv-download-", fileext = ".rds")
  download(url, destfile = destfile, quiet = TRUE)
  expect_equal(readRDS(destfile), list(a = 1))

})

test_that("zstd tarballs are detected from their contents", {

  renv_scope_tempdir()
  dir.create("pkg")
  writeLines(c("Package: pkg", "Version: 1.0"), con = "pkg/DESCRIPTION")

  # tar() only writes zstd when R (>= 4.5) was built with zstd support
  status <- catch(tar("pkg.tar.zst", files = "pkg", compression = "zstd", tar = "internal"))
  skip_if(inherits(status, "error"), "R was built without zstd support")

  # R's internal tar reads zstd whenever gzfile() does
  renv_scope_envvars(TAR = "internal")

  file.copy("pkg.tar.zst", "pkg.bin")
  expect_equal(renv_archive_type("pkg.bin"), "tar")
  expect_true("pkg/DESCRIPTION" %in% renv_archive_list("pkg.bin"))

  # a zstd-compressed non-tarball isn't an archive
  saveRDS(list(a = 1), "database.rds", compress = "zstd")
  expect_equal(renv_archive_type("database.rds"), "unknown")
  expect_false(renv_download_check_archive("database.rds"))

})
