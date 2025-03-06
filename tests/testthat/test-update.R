
test_that("update() finds packages requiring updates from CRAN", {

  skip_on_cran()

  renv_tests_scope()
  init()

  install("breakfast@0.1.0")
  expect_true(renv_package_version("breakfast") == "0.1.0")

  update()
  expect_true(renv_package_version("breakfast") == "1.0.0")

})

test_that("update() can upgrade GitHub packages", {

  skip_if(getRversion() < "3.5.3")
  skip_on_cran()
  skip_if_no_github_auth()
  skip_slow()

  renv_tests_scope()
  init()

  # download old commit from GitHub and track master
  install("kevinushey/skeleton@5fd5d3bc616794f869e47fdf3a8b4bcaa2afcf53")

  pkgpath <- find.package("skeleton")
  descpath <- file.path(pkgpath, "DESCRIPTION")

  dcf <- renv_dcf_read(descpath)
  expect_true(dcf$Version == "0.0.0.9000")

  remotes <- dcf[grep("^Remote", names(dcf))]
  remotes$RemoteRef <- "master"
  renv_package_augment(pkgpath, remotes)

  # try updating
  update(packages = "skeleton")

  # check for new version of package
  dcf <- renv_dcf_read(descpath)
  expect_true(dcf$Version == "1.0.1")

})

test_that("update() can upgrade Git packages", {

  skip_if(getRversion() < "3.5.3")
  skip_on_cran()
  skip_if_no_github_auth()
  skip_slow()

  # this test appears to fail on CI (ssh clone from GitHub disallowed?)
  testthat::skip_on_ci()

  renv_tests_scope()
  init()

  # download old commit from GitHub and track master
  local({
    renv_scope_envvars(RENV_AUTOLOADER_ENABLED = FALSE)
    remotes::install_git(
      url          = "https://github.com/kevinushey/skeleton",
      ref          = "5fd5d3bc616794f869e47fdf3a8b4bcaa2afcf53",
      quiet        = TRUE,
      INSTALL_opts = "--no-multiarch"
    )
  })

  pkgpath <- find.package("skeleton")
  descpath <- file.path(pkgpath, "DESCRIPTION")

  dcf <- renv_dcf_read(descpath)
  expect_true(dcf$Version == "0.0.0.9000")

  remotes <- dcf[grep("^Remote", names(dcf))]
  remotes$RemoteRef <- "master"
  renv_package_augment(pkgpath, remotes)

  # try updating
  update(packages = "skeleton")

  # check for new version of package
  dcf <- renv_dcf_read(descpath)
  expect_true(dcf$Version == "1.0.1")

})

test_that("can upgrade bitbucket", {

  skip_on_cran()

  latest <- outdated <- renv_remotes_resolve("bitbucket::kevinushey/skeleton")
  outdated$Version <- "1.0.0"
  outdated$RemoteSha <- "5fd5d3b"

  updated <- renv_update_find(list(outdated))

  expect_equal(updated$skeleton$Version, latest$Version)
  expect_equal(updated$skeleton$RemoteSha, latest$RemoteSha)

})

test_that("can upgrade gitlab", {

  skip_on_cran()

  latest <- outdated <- renv_remotes_resolve("gitlab::kevinushey/skeleton")
  outdated$Version <- "1.0.0"
  outdated$RemoteSha <- "5fd5d3b"

  updated <- renv_update_find(list(outdated))

  expect_equal(updated$skeleton$Version, latest$Version)
  expect_equal(updated$skeleton$RemoteSha, latest$RemoteSha)

})

test_that("we guard against invalid mc.cores values", {

  skip_on_windows()
  renv_scope_options(renv.config.updates.parallel = TRUE)

  local({
    renv_scope_options(mc.cores = 4L)
    expect_equal(renv_parallel_cores(), 4L)
  })

  local({
    renv_scope_options(mc.cores = NA)
    expect_equal(renv_parallel_cores(), 1L)
  })

  local({
    renv_scope_options(mc.cores = "oops")
    expect_equal(renv_parallel_cores(), 1L)
  })

})
