
# TODO: This test assumes 'pak' integration is disabled?
test_that("install works when DESCRIPTION contains no dependencies", {
  renv_tests_scope()
  desc <- c("Type: Package", "Package: test", "Version: 1.0")
  writeLines(desc, con = "DESCRIPTION")
  expect_length(install(), 0L)
})

test_that("requested version in DESCRIPTION file is honored", {

  renv_tests_scope()

  desc <- c(
    "Type: Package",
    "Package: test",
    "Imports: bread (== 0.1.0), toast"
  )
  writeLines(desc, con = "DESCRIPTION")

  install()

  expect_true(renv_package_version("bread") == "0.1.0")

})

test_that("installation failure is well-reported", {

  # TODO: test seems to fail because a connection gets
  # left open by utils::package.skeleton()
  skip_on_os("windows")

  renv_scope_tempdir()

  # init dummy library
  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)

  # dummy environment
  envir <- new.env(parent = emptyenv())
  envir[["hello"]] <- function() {}

  # prepare dummy package
  package <- "renv.dummy.package"
  unlink(package, recursive = TRUE)
  suppressMessages(utils::package.skeleton(package, environment = envir))

  # remove broken man files
  unlink("renv.dummy.package/Read-and-delete-me")
  unlink("renv.dummy.package/man", recursive = TRUE)

  # give the package a build-time error
  writeLines("parse error", con = file.path(package, "R/error.R"))

  # try to build it and confirm error
  record <- list(Package = package, Path = package)
  expect_error(renv_install_package_impl(record))

})

test_that("install forces update of dependencies as needed", {

  # TODO: this fails on CRAN presumedly because the wrong
  # version of the breakfast package is searched for; need
  # to figure out where the repositories are getting changed.
  skip_on_cran()
  renv_tests_scope("breakfast")

  # install the breakfast package
  install("breakfast")

  # ensure its dependencies were installed
  packages <- c("bread", "oatmeal", "toast")
  for (package in packages)
    expect_true(file.exists(renv_package_find(package)))

  # remove breakfast
  remove("breakfast")

  # modify 'toast' so that it's now too old
  path <- renv_package_find("toast")
  descpath <- file.path(path, "DESCRIPTION")
  desc <- renv_description_read(descpath)
  desc$Version <- "0.1.0"
  renv_dcf_write(desc, file = descpath)

  # try to install 'breakfast' again
  install("breakfast")

  # validate that 'toast' was updated to 1.0.0
  desc <- renv_description_read(package = "toast")
  expect_equal(desc$Version, "1.0.0")

})

test_that("packages can be installed from sources", {

  renv_tests_scope()
  init()

  # get path to package sources in local repos
  repos <- getOption("repos")[["CRAN"]]
  tarball <- file.path(repos, "src/contrib/bread_1.0.0.tar.gz")

  # try to install it
  install(tarball)
  expect_true(renv_package_version("bread") == "1.0.0")

})

test_that("various remote styles can be used during install", {
  skip_if_no_github_auth()

  renv_tests_scope()
  init()

  # install CRAN latest
  install("bread")
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_version("bread") == "1.0.0")

  # install from archive
  install("bread@0.1.0")
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_version("bread") == "0.1.0")

  # install from github
  install("kevinushey/skeleton")
  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.1")

  # install from github PR
  install("kevinushey/skeleton#1")
  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.2")

  # install from branch
  install("kevinushey/skeleton@feature/version-bump")
  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.2")

  # install from subdir
  install("kevinushey/subdir:subdir")
  expect_true(renv_package_installed("subdir"))
  expect_true(renv_package_version("subdir") == "0.0.0.9000")

  # install from URL to zip
  install("https://github.com/kevinushey/skeleton/archive/master.zip")
  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.1")

})

test_that("Remotes fields in a project DESCRIPTION are respected", {
  skip_if_no_github_auth()

  renv_tests_scope()
  init()

  desc <- c(
    "Type: Package",
    "Package: renv.test.package",
    "Suggests: skeleton",
    "Remotes: kevinushey/skeleton"
  )

  writeLines(desc, con = "DESCRIPTION")
  install()

  record <- renv_snapshot_description(package = "skeleton")
  expect_true(record$Source == "GitHub")

})

test_that("source packages in .zip files can be installed", {

  renv_tests_scope()

  location <- download.packages("bread", destdir = renv_scope_tempfile())

  path <- location[1, 2]
  renv_archive_decompress(path, exdir = "bread")

  zippath <- file.path(getwd(), "bread_1.0.0.zip")
  setwd("bread")
  status <- catchall(zip(zippath, files = ".", extras = "-q"))
  setwd("..")

  if (inherits(status, "condition"))
    skip("could not zip archive")

  install(zippath)
  expect_true(renv_package_installed("bread"))

})

test_that("renv warns when installing an already-loaded package", {
  skip_on_cran()
  renv_tests_scope()
  install("bread@1.0.0")
  renv_namespace_load("bread")
  defer(renv_namespace_unload("bread"))

  expect_snapshot(install("bread@0.1.0"))

})

test_that("install() writes out Github fields for backwards compatibility", {
  skip_if_no_github_auth()
  renv_tests_scope()

  install("rstudio/packrat")
  descpath <- file.path(.libPaths()[1], "packrat/DESCRIPTION")
  dcf <- renv_description_read(descpath)

  expect_equal(dcf$RemoteRepo,     dcf$GithubRepo)
  expect_equal(dcf$RemoteUsername, dcf$GithubUsername)
  expect_equal(dcf$RemoteRef,      dcf$GithubRef)
  expect_equal(dcf$RemoteSha,      dcf$GithubSHA1)

})

test_that("renv uses safe library paths on Windows", {
  skip_if_not(renv_platform_windows())
  renv_tests_scope()

  goodlib <- "Research and Development"
  expect_true(renv_libpaths_safe(goodlib) == goodlib)

  badlib <- "R&D"
  expect_false(renv_libpaths_safe(badlib) != badlib)

  ensure_directory(badlib)
  renv_libpaths_set(badlib)
  install("bread")

  descpath <- file.path(getwd(), "R&D/bread")
  desc <- renv_description_read(descpath)

  expect_true(desc$Package == "bread")
  expect_true(desc$Version == "1.0.0")

})

test_that("renv uses safe library path when needed", {

  renv_tests_scope()

  badlib <- file.path(getwd(), "Has'Single'Quote")
  dir.create(badlib)
  expect_false(renv_libpaths_safe(badlib) == badlib)

})

test_that("renv can install packages from Bitbucket", {
  skip_on_cran()
  renv_tests_scope()
  install("bitbucket::kevinushey/skeleton")
  expect_true(renv_package_installed("skeleton"))
})

test_that("install via version succeeds", {
  skip_on_cran()
  renv_tests_scope()

  install("bread@0.0.1")
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_version("bread") == "0.0.1")

})

test_that("install() installs inferred dependencies", {

  skip_on_cran()
  renv_tests_scope("breakfast")

  # try installing packages
  records <- install()

  # validate that we've installed breakfast + deps
  expect_length(records, 4L)
  expect_true(renv_package_installed("breakfast"))

  # try calling install once more; nothing should happen
  records <- install()
  expect_length(records, 0L)

})

test_that("install() prefers cellar when available", {

  skip_on_cran()
  renv_tests_scope()

  locals <- paste(
    renv_tests_path("nowhere"),
    renv_tests_path("local"),
    sep = ";"
  )

  renv_scope_options(renv.config.cache.enabled = FALSE)
  renv_scope_envvars(RENV_PATHS_CELLAR = locals)

  records <- install("skeleton")

  record <- records$skeleton
  expect_equal(record$Source, "Cellar")

  path <- renv_path_normalize(renv_tests_path("local/skeleton"))
  prefix <- if (renv_platform_windows()) "file:///" else "file://"
  uri <- paste0(prefix, path)

  expect_equal(attr(record, "url"), uri)

})

test_that("packages can be installed from the archive w/libcurl", {
  skip_on_cran()

  # validate that we have libcurl
  ok <- identical(capabilities("libcurl"), c(libcurl = TRUE))
  skip_if(!ok, "libcurl is not available")

  # perform test
  renv_tests_scope()
  renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
  install("bread@0.1.0")
  expect_true(renv_package_installed("bread"))
  expect_equal(renv_package_version("bread"), "0.1.0")

})

test_that("issue #609", {
  skip_on_cran()

  renv_tests_scope()

  renv_scope_options(configure.vars = c(breakfast = ""))
  install("bread")
  expect_true(renv_package_installed("bread"))
})

test_that("we can install packages from git remotes within subdirs", {

  skip_on_cran()
  skip_on_ci()
  skip("unreliable test")

  renv_tests_scope("subdir")

  install("git@github.com:kevinushey/subdir.git:subdir", rebuild = TRUE)
  expect_true(renv_package_installed("subdir"))

  snapshot()

  remove("subdir")
  expect_false(renv_package_installed("subdir"))

  restore(packages = "subdir", rebuild = TRUE)
  expect_true(renv_package_installed("subdir"))

})

test_that("packages embedded in the project use a project-local RemoteURL", {

  skip_if(getRversion() < "4.1")
  skip_if_not_installed("usethis")

  renv_tests_scope("example")

  usethis <- renv_namespace_load("usethis")
  skip_if(is.null(usethis$create_package))
  renv_scope_options(usethis.quiet = TRUE)
  unlink("example", recursive = TRUE)
  usethis$create_package("example", rstudio = FALSE, open = FALSE)

  install("./example")
  lockfile <- snapshot(lockfile = NULL)
  expect_equal(lockfile$Packages$example$RemoteUrl, "./example")

  # TODO: if the user provides a "weird" path, we'll use it as-is.
  # is that okay? what about relative paths that resolve outside of
  # the project root directory?
  install("./././example")
  lockfile <- snapshot(lockfile = NULL)
  expect_equal(lockfile$Packages$example$RemoteUrl, "./././example")

})

test_that("packages installed from cellar via direct path", {

  skip_on_cran()
  renv_tests_scope("skeleton")

  locals <- paste(
    renv_tests_path("nowhere"),
    renv_tests_path("local"),
    sep = ";"
  )

  renv_scope_options(renv.config.cache.enabled = FALSE)
  renv_scope_envvars(RENV_PATHS_CELLAR = locals)

  path <- renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")
  records <- install(path, rebuild = TRUE)
  expect_equal(records$skeleton$Source, "Cellar")

  lockfile <- snapshot(lockfile = NULL)
  expect_equal(lockfile$Packages$skeleton$Source, "Cellar")

})

test_that("staging library path has same permissions as library path", {

  skip_on_cran()
  skip_on_windows()

  renv_tests_scope()

  library <- renv_paths_library()
  ensure_directory(library)
  renv_scope_libpaths(library)

  umask <- Sys.umask("0")
  Sys.chmod(library, "0775")
  Sys.umask(umask)

  staging <- renv_install_staged_library_path()
  expect_equal(file.mode(staging), file.mode(library))

})

test_that("packages installed from a RemoteSubdir can be retrieved from cache", {

  skip_on_windows()
  skip_slow()

  renv_tests_scope()
  cachepath <- renv_scope_tempfile("renv-cache-")
  ensure_directory(cachepath)
  renv_scope_envvars(RENV_PATHS_CACHE = cachepath)

  init()

  # install first from remote
  install("kevinushey/subdir:subdir")

  # remove, and re-install from cache
  remove("subdir")
  install("kevinushey/subdir:subdir")

  expect_true(renv_package_installed("subdir"))

})

test_that("repositories containing multiple packages can be installed", {

  skip_on_windows()
  skip_slow()

  renv_tests_scope()

  install("kevinushey/subdir:pkgA")
  expect_true(renv_package_installed("pkgA"))

  install("kevinushey/subdir:pkgB")
  expect_true(renv_package_installed("pkgB"))

})

test_that("Suggest dependencies are used when requested", {

  renv_tests_scope("breakfast")
  fields <- c("Imports", "Depends", "LinkingTo", "Suggests")
  settings$package.dependency.fields(fields)
  install("breakfast")
  expect_true(renv_package_installed("egg"))

})

test_that("custom dependency fields in install are supported", {

  skip_on_cran()
  skip_on_windows()

  renv_tests_scope()

  install("breakfast", dependencies = "strong")
  expect_false(renv_package_installed("egg"))

  install("breakfast", dependencies = c("strong", "Config/Needs/protein"))
  expect_true(renv_package_installed("egg"))
})

test_that("install has user-friendly output", {

  renv_scope_libpaths()
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-tempcache-"))

  renv_tests_scope("breakfast")
  expect_snapshot(install())

  renv_tests_scope("breakfast")
  expect_snapshot(install())

})

test_that("package sources of the form <pkg>_<sha>.zip can be installed", {
  skip_on_cran()
  skip_if(!renv_platform_windows())

  renv_tests_scope()
  renv_tests_scope_repos()

  # get path to .tar.gz
  source <- download.packages("bread", type = "source")[1, 2]

  # repack as a .zip archive
  exdir <- renv_scope_tempfile("bread-")
  ensure_directory(exdir)
  renv_archive_decompress(source, exdir = exdir)

  zipfile <- file.path(tempdir(), "bread_f96a78e23d44d68d329c2dbf168a4dee1882a1c6.zip")
  local({
    renv_scope_wd(exdir)
    zip(zipfile, files = "bread")
  })

  # now try to install it
  install(zipfile)
  expect_true(renv_package_installed("bread"))

})

test_that("package binaries of the form <pkg>_<sha>.zip can be installed", {
  skip_on_cran()
  skip_if(!renv_platform_windows())

  renv_tests_scope()
  renv_tests_scope_repos()

  # install bread
  install("bread")

  # create a zipfile from the installed package
  library <- renv_libpaths_active()
  zipfile <- file.path(tempdir(), "bread_f96a78e23d44d68d329c2dbf168a4dee1882a1c6.zip")
  local({
    renv_scope_wd(library)
    zip(zipfile, files = "bread", extras = "-q")
  })

  # remove bread
  remove("bread")
  expect_false(renv_package_installed("bread"))

  # now try to install from zipfile
  install(zipfile)
  expect_true(renv_package_installed("bread"))

})

test_that("install() reports failure when a 'bad' binary is installed", {

  skip_on_cran()
  renv_tests_scope()

  # test package load in this scope on all platforms
  renv_scope_envvars(RENV_INSTALL_TEST_LOAD = TRUE)

  # install bread
  install("bread")

  # copy the installed package, and create a 'broken' binary
  src <- renv_package_find("bread")
  tgt <- file.path(tempdir(), "bread")
  renv_file_copy(src, tgt)

  local({
    renv_scope_wd(tgt)
    dir.create("R")
    writeLines("stop('oh no')", con = "R/bread")
  })

  # try installing the broken binary
  remove("bread")
  expect_false(renv_package_installed("bread"))
  expect_error(install(tgt))
  expect_false(renv_package_installed("bread"))

  # try skipping the load test
  renv_scope_options(INSTALL_opts = c(bread = "--no-test-load"))
  install(tgt)
  expect_true(renv_package_installed("bread"))
  expect_error(renv_namespace_load(bread))
  remove("bread")

})

test_that("install() respects dependencies argument", {
  skip_on_cran()
  project <- renv_tests_scope()
  init()

  contents <- heredoc("
    Type: Project
    Depends: coffee
    Imports: bread
    Suggests: muffin
  ")

  writeLines(contents, con = "DESCRIPTION")
  install(dependencies = "Imports")

  expect_true(renv_package_installed("bread"))
  expect_false(renv_package_installed("coffee"))
  expect_false(renv_package_installed("muffin"))
})

test_that("install() succeeds even some repositories cannot be queried", {
  renv_tests_scope()

  repos <- getOption("repos")
  repos[["NARC"]] <- file.path(repos[["CRAN"]], "missing")
  renv_scope_options(repos = repos)

  init()
  install("bread")
  expect_true(renv_package_installed("bread"))
})

test_that("install() doesn't duplicate authentication headers", {
  renv_scope_envvars(RENV_DOWNLOAD_METHOD = "libcurl")
  project <- renv_tests_scope()
  init()
  install("kevinushey/skeleton")
  expect_true(renv_package_installed("skeleton"))
})

test_that("install() stores repository information for installed packages", {

  project <- renv_tests_scope(isolated = TRUE)
  init()

  # unset repository option
  repos <- getOption("repos")
  renv_scope_options(repos = character())

  # try to install a package
  writeLines("library(bread)", con = "_deps.R")
  install("bread", repos = c(TEST = unname(repos)))

  # create a lockfile
  snapshot()

  # validate that the repository information is stored
  lockfile <- renv_lockfile_read("renv.lock")
  record <- lockfile$Packages$bread
  expect_equal(!!record$Source, "Repository")
  expect_equal(!!record$Repository, !!unname(repos))

  # now, add the repository back; it should then be aliased in lockfile
  options(repos = c(TEST = unname(repos)))
  snapshot()

  lockfile <- renv_lockfile_read("renv.lock")
  record <- lockfile$Packages$bread
  expect_equal(!!record$Source, "Repository")
  expect_equal(!!record$Repository, "TEST")

})

test_that("install() lazily resolves project remotes", {

  project <- renv_tests_scope()
  init()

  writeLines("Remotes: kevinushey/skeleton", con = "DESCRIPTION")
  install("bread")
  expect_false(renv_package_installed("skeleton"))

})

test_that("install() records the repository used to retrieve a package", {

  project <- renv_tests_scope(isolated = TRUE)
  init()

  url <- unname(getOption("repos"))
  local({
    renv_scope_options(repos = character())
    install("bread", repos = c(TEST = url), rebuild = TRUE)
  })

  dcf <- renv_description_read(package = "bread")
  expect_equal(!!dcf$RemoteRepos, !!url)
  expect_equal(!!dcf$RemoteReposName, "TEST")

})

test_that("recursive dependency versions are properly resolved", {

  project <- renv_tests_scope()
  init()

  install("phone@0.1.0")
  install("jamie")

  expect_equal(renv_package_version("phone"), "1.0.0")

})

test_that("install(lock = TRUE) updates lockfile", {

  project <- renv_tests_scope()
  init()

  # first, get a lockfile as created via install + lock
  install("breakfast", lock = TRUE)
  actual <- renv_lockfile_load(project = project)

  # now, try re-computing the lockfile via snapshot
  writeLines("library(breakfast)", con = "dependencies.R")
  expected <- snapshot(lockfile = NULL)

  # make sure they're equal
  expect_mapequal(
    renv_lockfile_records(actual),
    renv_lockfile_records(expected)
  )

})
