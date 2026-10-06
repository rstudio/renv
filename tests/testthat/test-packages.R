
test_that("remote field updates are written to both DESCRIPTION, packages.rds", {

  url <- renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")

  record <- list(
    Package    = "skeleton",
    Version    = "1.0.1",
    Source     = "local",
    RemoteUrl  = url
  )

  renv_tests_scope()
  renv_scope_envvars(RENV_PATHS_LOCAL = NULL)
  install(packages = list(record))

  pkgpath <- renv_package_find("skeleton")

  descpath <- file.path(pkgpath, "DESCRIPTION")
  desc <- renv_description_read(descpath)

  metapath <- file.path(pkgpath, "Meta/package.rds")
  meta <- as.list(readRDS(metapath)$DESCRIPTION)

  expect_true(desc$RemoteType == "local")
  expect_true(meta$RemoteType == "local")

  expect_true(desc$RemoteUrl == url)
  expect_true(meta$RemoteUrl == url)

})

test_that("package archive file names are parsed into their components", {

  paths <- c(
    "/cellar/bread_1.0.0.tar.gz",
    "toast_2.1-3.tgz",
    "oatmeal_0.9.zip",
    "base64enc_0.1-6_R_macos-arm64.tar.xz",
    "pkg_1.0_R_x86_64-pc-linux-gnu.tar.gz",
    "data.table_1.14.8_R_windows-clang-aarch64.tar.zstd",
    "README.md",
    "notes.tar.xz"
  )

  parsed <- renv_package_filename_parse(paths)

  expect_equal(
    parsed$Package,
    c("bread", "toast", "oatmeal", "base64enc", "pkg", "data.table")
  )

  expect_equal(
    parsed$Version,
    c("1.0.0", "2.1-3", "0.9", "0.1-6", "1.0", "1.14.8")
  )

  expect_equal(
    parsed$Build,
    c(NA, NA, NA, "macos-arm64", "x86_64-pc-linux-gnu", "windows-clang-aarch64")
  )

  expect_equal(
    parsed$Ext,
    c(".tar.gz", ".tgz", ".zip", ".tar.xz", ".tar.gz", ".tar.zstd")
  )

  expect_equal(parsed$Path, paths[1:6])

  empty <- renv_package_filename_parse(character())
  expect_equal(nrow(empty), 0L)

})

test_that("the package extension pattern matches every known extension", {

  exts <- renv_package_extensions()
  files <- paste0("pkg_1.0", exts)
  expect_true(all(grepl(renv_package_ext_pattern(), files, perl = TRUE)))
  expect_false(grepl(renv_package_ext_pattern(), "pkg_1.0.tar", perl = TRUE))
  expect_false(grepl(renv_package_ext_pattern(), "pkg_1.0.rds", perl = TRUE))

  expect_equal(renv_package_extensions("source"), ".tar.gz")
  expect_true(renv_package_ext("binary") %in% renv_package_extensions("binary"))

})
