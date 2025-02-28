
test_that("snapshot is idempotent", {

  renv_tests_scope("oatmeal")

  init(bare = TRUE)
  install("oatmeal")
  snapshot()
  before <- renv_lockfile_read("renv.lock")
  snapshot()
  after <- renv_lockfile_read("renv.lock")
  expect_equal(before, after)

})

test_that("snapshot failures are reported", {

  renv_tests_scope("oatmeal")
  init()

  descpath <- system.file("DESCRIPTION", package = "oatmeal")
  unlink(descpath)
  expect_snapshot(snapshot())

})

test_that("broken symlinks are reported", {
  skip_on_os("windows")

  renv_scope_envvars(RENV_PATHS_ROOT = renv_scope_tempfile())
  renv_tests_scope("oatmeal")
  init()

  oatmeal <- renv_path_normalize(system.file(package = "oatmeal"))
  unlink(oatmeal, recursive = TRUE)
  expect_snapshot(snapshot())

})

test_that("multiple libraries can be used when snapshotting", {

  renv_scope_envvars(RENV_PATHS_ROOT = renv_scope_tempfile())
  renv_tests_scope()

  init()

  lib1 <- renv_scope_tempfile("renv-lib1-")
  lib2 <- renv_scope_tempfile("renv-lib2-")
  ensure_directory(c(lib1, lib2))

  oldlibpaths <- .libPaths()
  .libPaths(c(lib1, lib2))

  install("bread", library = lib1)
  breadloc <- find.package("bread")
  expect_true(renv_file_same(dirname(breadloc), lib1))

  install("toast", library = lib2)
  toastloc <- find.package("toast")
  expect_true(renv_file_same(dirname(toastloc), lib2))

  libs <- c(lib1, lib2)
  lockfile <- snapshot(lockfile = NULL, library = libs, type = "all")
  records <- renv_lockfile_records(lockfile)

  expect_length(records, 2L)
  expect_setequal(names(records), c("bread", "toast"))

  .libPaths(oldlibpaths)

})

test_that("implicit snapshots only include packages currently used", {

  renv_tests_scope("oatmeal")
  init()

  # install toast, but don't declare that we use it
  install("toast")
  lockfile <- snapshot(type = "implicit", lockfile = NULL)
  records <- renv_lockfile_records(lockfile)
  expect_length(records, 1L)
  expect_setequal(names(records), "oatmeal")

  # use toast
  writeLines("library(toast)", con = "toast.R")
  lockfile <- snapshot(type = "packrat", lockfile = NULL)
  records <- renv_lockfile_records(lockfile)
  expect_length(records, 3L)
  expect_setequal(names(records), c("oatmeal", "bread", "toast"))

})

test_that("explicit snapshots only capture packages in DESCRIPTION", {

  renv_tests_scope("breakfast")
  init()

  desc <- list(Type = "Project", Depends = "toast")

  write.dcf(desc, file = "DESCRIPTION")
  lockfile <- snapshot(type = "explicit", lockfile = NULL)
  records <- renv_lockfile_records(lockfile)
  expect_true(length(records) == 2L)
  expect_true(!is.null(records[["bread"]]))
  expect_true(!is.null(records[["toast"]]))

})

test_that("a custom snapshot filter can be used", {
  skip_on_cran()
  renv_tests_scope("breakfast")

  settings$snapshot.type("custom")
  filter <- function(project) c("bread", "toast")
  renv_scope_options(renv.snapshot.filter = filter)

  init()
  lockfile <- renv_lockfile_load(project = getwd())
  expect_setequal(names(renv_lockfile_records(lockfile)), c("bread", "toast"))

})

test_that("snapshotted packages from CRAN include the Repository field", {

  renv_tests_scope("bread")
  init()

  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_lockfile_records(lockfile)
  expect_true(records$bread$Repository == "CRAN")

})

test_that("snapshot failures due to bad library / packages are reported", {
  project <- renv_tests_scope("oatmeal")
  init()
  isolate()
  writeLines("invalid", con = system.file("DESCRIPTION", package = "oatmeal"))
  expect_error(snapshot())
})

test_that("snapshot ignores own package in package development scenarios", {

  renv_tests_scope()
  ensure_directory("bread")
  renv_scope_wd("bread")

  writeLines(c("Type: Package", "Package: bread"), con = "DESCRIPTION")

  ensure_directory("R")
  writeLines("function() { library(bread) }", con = "R/deps.R")

  lockfile <- snapshot(lockfile = NULL)
  records <- renv_lockfile_records(lockfile)
  expect_true(is.null(records[["bread"]]))

})

test_that("snapshot warns about unsatisfied dependencies", {

  renv_tests_scope("toast")
  init(settings = list(use.cache = FALSE))

  descpath <- system.file("DESCRIPTION", package = "toast")
  toast <- renv_description_read(descpath)
  toast$Depends <- "bread (> 1.0.0)"
  renv_dcf_write(toast, file = descpath)

  expect_snapshot(snapshot(), error = TRUE)

})

test_that("snapshot records packages discovered in cellar", {

  renv_tests_scope("skeleton")
  renv_scope_envvars(
    RENV_PATHS_CACHE = renv_scope_tempfile(),
    RENV_PATHS_LOCAL = renv_tests_path("local")
  )

  init(bare = TRUE)

  record <- list(Package = "skeleton", Version = "1.0.1")
  records <- install(list(record))

  # validate the record in the lockfile
  lockfile <- snapshot(lockfile = NULL)
  records <- renv_lockfile_records(lockfile)
  skeleton <- records[["skeleton"]]

  expect_equal(skeleton$Package, "skeleton")
  expect_equal(skeleton$Version, "1.0.1")
  expect_equal(skeleton$Source, "Cellar")

})

test_that("snapshot prefers RemoteType to biocViews", {

  desc <- list(
    Package = "test",
    Version = "1.0",
    RemoteType = "github",
    biocViews = "Biology"
  )

  descfile <- renv_scope_tempfile()
  renv_dcf_write(desc, file = descfile)
  record <- renv_snapshot_description(descfile)
  expect_identical(record$Source, "GitHub")

})

test_that("parse errors cause snapshot to abort", {

  renv_tests_scope()

  # write invalid code to an R file
  writeLines("parse error", con = "parse-error.R")

  # init should succeed even with parse errors
  init(bare = TRUE)

  # snapshots should fail when configured to do so
  renv_scope_options(renv.config.dependency.errors = "fatal")
  expect_error(snapshot())

})

test_that("records for packages available on other OSes are preserved", {
  skip_on_os("windows")
  renv_tests_scope("unixonly")

  init()

  # fake a windows-only record
  lockfile <- renv_lockfile_read("renv.lock")
  lockfile$Packages$windowsonly <- lockfile$Packages$unixonly
  lockfile$Packages$windowsonly$Package <- "windowsonly"
  lockfile$Packages$windowsonly$Hash <- NULL
  lockfile$Packages$windowsonly$OS_type <- "windows"
  renv_lockfile_write(lockfile, "renv.lock")

  # call snapshot to update lockfile
  snapshot()

  # ensure that 'windowsonly' is still preserved
  lockfile <- renv_lockfile_read("renv.lock")
  expect_true(!is.null(lockfile$Packages$windowsonly))

})

test_that(".renvignore works during snapshot without an explicit root", {

  renv_tests_scope()

  # install bread
  install("bread")

  # create sub-directory that should be ignored
  dir.create("ignored")
  writeLines("library(bread)", con = "ignored/script.R")

  lockfile <- snapshot(project = ".", lockfile = NULL)
  expect_false(is.null(lockfile$Packages$bread))

  writeLines("*", con = "ignored/.renvignore")

  lockfile <- snapshot(project = ".", lockfile = NULL)
  expect_true(is.null(lockfile$Packages$bread))

})

test_that("snapshot(packages = ...) captures package dependencies", {

  renv_tests_scope("breakfast")

  # init to install required packages
  init()

  # remove old lockfile
  unlink("renv.lock")

  # create lockfile
  snapshot(packages = "breakfast")

  # check for expected records
  lockfile <- renv_lockfile_load(project = getwd())
  records <- renv_lockfile_records(lockfile)

  expect_true(!is.null(records$breakfast))
  expect_true(!is.null(records$bread))
  expect_true(!is.null(records$toast))
  expect_true(!is.null(records$oatmeal))

})

test_that("snapshot() accepts relative library paths", {

  renv_tests_scope("breakfast")

  # initialize project
  init()

  # remove lockfile
  unlink("renv.lock")

  # form relative path to library
  library <- substring(.libPaths()[1], nchar(getwd()) + 2)

  # try to snapshot with relative library path
  snapshot(library = library)

  # test that snapshot succeeded
  expect_true(file.exists("renv.lock"))

})

test_that("snapshot(update = TRUE) preserves old records", {

  skip_on_cran()
  skip_if_no_github_auth()

  renv_tests_scope("breakfast")
  init()

  # remove breakfast, then try to snapshot again
  old <- renv_lockfile_read("renv.lock")
  remove("breakfast")
  snapshot(update = TRUE)
  new <- renv_lockfile_read("renv.lock")

  expect_identical(names(old$Packages), names(new$Packages))

  # try installing a package
  old <- renv_lockfile_read("renv.lock")
  writeLines("library(halloween)", con = "halloween.R")
  install("halloween")
  snapshot(update = TRUE)
  new <- renv_lockfile_read("renv.lock")

  # check that we have our old record names
  expect_true(all(old$Packages %in% new$Packages))

  # now try removing 'breakfast'
  snapshot(update = FALSE)
  new <- renv_lockfile_read("renv.lock")
  expect_false("breakfast" %in% names(new$Packages))

})

test_that("renv reports missing packages in explicit snapshots", {

  renv_tests_scope()
  init()

  writeLines("Depends: breakfast", con = "DESCRIPTION")
  expect_snapshot(snapshot(type = "explicit"))

})

test_that("a project using explicit snapshots is marked in sync appropriately", {

  skip_on_cran()
  renv_tests_scope()
  renv_scope_options(renv.config.snapshot.type = "explicit")

  init()

  writeLines("Depends: breakfast", con = "DESCRIPTION")
  expect_false(status()$synchronized)

  install("breakfast")
  expect_false(status()$synchronized)

  snapshot()
  expect_true(status()$synchronized)

})

test_that("snapshot() warns when required package is not installed", {

  renv_tests_scope("breakfast")
  init()

  remove("breakfast")
  expect_snapshot(snapshot())

  install("breakfast")
  remove("toast")
  expect_snapshot(snapshot(), error = TRUE)

})

test_that("packages installed from CRAN using pak are handled", {
  skip_on_cran()
  skip_if_not_installed("pak")

  renv_tests_scope()
  library <- renv_paths_library()
  ensure_directory(library)
  pak <- renv_namespace_load("pak")
  quietly(pak$pkg_install("toast"))
  record <- renv_snapshot_description(package = "toast")

  expected <- c("Package", "Version", "Source", "Repository")
  expect_contains(names(record), expected)

  expect_identical(record$Source, "Repository")
  expect_identical(record$Repository, "CRAN")
})


test_that("packages installed from Bioconductor using pak are handled", {
  skip_on_cran()
  skip_if_not_installed("pak")
  skip_if(devel())

  renv_tests_scope()
  library <- renv_paths_library()
  ensure_directory(library)
  pak <- renv_namespace_load("pak")
  suppressMessages(pak$pkg_install("bioc::Biobase"))

  record <- renv_snapshot_description(package = "Biobase")
  expect_identical(record$Source, "Bioconductor")
})

test_that("snapshot always reports on R version changes", {
  renv_scope_options(renv.verbose = TRUE)

  R4.1 <- list(R = list(Version = 4.1))
  R4.2 <- list(R = list(Version = 4.2))
  expect_snapshot({
    renv_snapshot_report_actions(list(), R4.1, R4.2)
  })
})

test_that("user can choose to install missing packages", {

  # use a temporary cache to guarantee packages are fully installed
  # regardless of order other tests are run in
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-tempcache-"))

  renv_tests_scope("egg")
  renv_scope_options(renv.menu.choice = 2)
  expect_snapshot(snapshot())

})

test_that("exclude handles uninstalled packages", {
  project <- renv_tests_scope("bread")
  init()
  snapshot(exclude = "bread")
  lockfile <- renv_lockfile_load(project)
  expect_null(lockfile$Packages$bread)
})

test_that("snapshot doesn't include development dependencies", {

  renv_tests_scope()
  writeLines(c("Imports: egg", "Suggests: bread"), "DESCRIPTION")

  inst <- install()
  expect_named(inst, c("bread", "egg"), ignore.order = TRUE)

  snap <- snapshot()
  expect_named(snap$Packages, "egg")

})

test_that("automatic snapshot works as expected", {

  renv_scope_binding(the, "auto_snapshot_forced", TRUE)
  defer(the$library_info <- NULL)

  project <- renv_tests_scope("oatmeal")
  init()
  expect_silent(renv_snapshot_task())

  # bread isn't used so snapshot shouldn't change
  install("bread")
  expect_silent(renv_snapshot_task())

  writeLines("library(egg)", "dependencies.R")
  install("egg")
  expect_snapshot(renv_snapshot_task())

})

# test_that("we can infer github remotes from packages installed from sources", {
#   skip_on_cran()
#
#   desc <- heredoc("
#     Package: renv
#     Version: 0.1.0-9000
#     BugReports: https://github.com/rstudio/renv/issues
#   ")
#
#   descfile <- renv_scope_tempfile("description-")
#   writeLines(desc, con = descfile)
#
#   remote <- local({
#     renv_scope_options(renv.verbose = FALSE)
#     renv_snapshot_description(path = descfile)
#   })
#
#   expect_equal(remote$RemoteType, "github")
#
#   expect_snapshot(. <- renv_snapshot_description(path = descfile))
#
# })

test_that("we report if dependency discover during snapshot() is slow", {

  renv_tests_scope()
  init()

  renv_scope_options(renv.dependencies.elapsed_time_threshold = -1)
  expect_snapshot(. <- snapshot())

})

test_that("failures in automatic snapshots disable automatic snapshots", {

  renv_scope_binding(the, "auto_snapshot_forced", TRUE)
  defer(the$library_info <- NULL)

  project <- renv_tests_scope("bread")
  init()

  count <- 0L
  renv_scope_trace(renv:::renv_snapshot_auto, function() {
    count <<- count + 1L
    stop("simulated failure in snapshot task")
  })

  the$library_info <- list()
  expect_false(the$auto_snapshot_failed)
  expect_snapshot(renv_snapshot_task())
  expect_true(the$auto_snapshot_failed)

  the$library_info <- list()
  expect_silent(renv_snapshot_task())
  expect_equal(count, 1L)

})

# https://github.com/rstudio/renv/issues/1607
test_that("snapshot() reports missing packages even if renv.verbose is FALSE", {
  project <- renv_tests_scope()
  init()

  renv_scope_options(renv.verbose = FALSE)
  writeLines("library(bread)", con = "deps.R")
  expect_snapshot(. <- snapshot(force = TRUE))
})

test_that("packages installed from r-universe preserve remote metadata", {

  text <- heredoc("
    Package: skeleton
    Type: Package
    Version: 1.1.0
    Remotes: kevinushey/skeleton
    Repository: https://kevinushey.r-universe.dev
    RemoteUrl: https://github.com/kevinushey/skeleton
    RemoteSha: e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a
  ")

  path <- renv_scope_tempfile()
  writeLines(text, con = path)

  record <- renv_snapshot_description(path = path)
  expect_identical(record[["RemoteSha"]], "e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

})

test_that("standard remotes preserve RemoteSha if it's a hash", {

  text <- heredoc("
    Package: skeleton
    Type: Package
    Version: 1.1.0
    Remotes: kevinushey/skeleton
    Repository: https://kevinushey.r-universe.dev
    RemoteType: standard
    RemoteUrl: https://github.com/kevinushey/skeleton
    RemoteSha: e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a
  ")

  path <- renv_scope_tempfile()
  writeLines(text, con = path)

  record <- renv_snapshot_description(path = path)
  expect_identical(record[["RemoteSha"]], "e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

})

test_that("standard remotes drop RemoteSha if it's a version", {

  text <- heredoc("
    Package: skeleton
    Type: Package
    Version: 1.1.0
    Remotes: kevinushey/skeleton
    Repository: https://kevinushey.r-universe.dev
    RemoteType: standard
    RemoteSha: 1.1.0
  ")

  path <- renv_scope_tempfile()
  writeLines(text, con = path)

  record <- renv_snapshot_description(path = path)
  expect_null(record[["RemoteSha"]])

})

test_that("a package's hash can be re-generated from lockfile", {

  project <- renv_tests_scope("breakfast")
  init()

  lockfile <- snapshot(lockfile = NULL)
  records <- renv_lockfile_records(lockfile)

  enumerate(records, function(package, record) {
    path <- system.file("DESCRIPTION", package = package)
    actual <- renv_hash_description(path)
    expected <- renv_hash_record(record)
    expect_equal(actual, expected)
  })

})

test_that("lockfiles are stable (v1)", {

  renv_scope_options(renv.lockfile.version = 1L)

  project <- renv_tests_scope("breakfast")
  init()

  expect_snapshot(. <- writeLines(readLines("renv.lock")))

})

test_that("lockfiles are stable (v2)", {

  renv_scope_options(renv.lockfile.version = 2L)

  project <- renv_tests_scope("breakfast")
  init()

  expect_snapshot(. <- writeLines(readLines("renv.lock")))

})

# # https://github.com/rstudio/renv/issues/2073
test_that("empty .ipynb files are handled gracefully", {

  skip_on_cran()
  project <- renv_tests_scope("bread")
  init()

  writeLines("", con = "example.ipynb")
  snapshot()

})

test_that("invalid lockfiles don't prevent calls to snapshot", {

  skip_on_cran()
  project <- renv_tests_scope("bread")
  init()

  writeLines("whoops!", con = "renv.lock")
  suppressWarnings(snapshot())
  expect_true(TRUE)

})
