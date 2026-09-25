
test_that("renv_records_select() handles missing packages gracefully", {

  # simulate what happens during printing of records during install
  lhs <- list()
  rhs <- list(skeleton = list(Package = "skeleton"))

  actions <- c(skeleton = "install")
  action <- "install"

  expect_identical(renv_records_select(lhs, actions, action), lhs)
  expect_identical(renv_records_select(rhs, actions, action), rhs)

})

test_that("we can format records in various ways", {

  old <- list(
    Package    = "skeleton",
    Version    = "1.0.0",
    Source     = "Repository",
    Repository = "CRAN"
  )

  new <- list(
    Package        = "skeleton",
    Version        = "1.0.0",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton"
  )

  expect_equal(renv_record_format_short(old), "1.0.0")
  expect_equal(renv_record_format_short(new), "kevinushey/skeleton")

  expect_equal(
    renv_record_format_pair(old, new),
    "[1.0.0 -> kevinushey/skeleton]"
  )

  expect_equal(
    renv_record_format_pair(new, new),
    "[kevinushey/skeleton: unchanged]"
  )

  record <- list(
    Package        = "skeleton",
    Version        = "1.0.0",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteRef      = "feature/branch"
  )

  expect_equal(
    renv_record_format_short(record),
    "kevinushey/skeleton@feature/branch"
  )

  old <- list(
    Package        = "skeleton",
    Version        = "1.0.0",
    Source         = "GitHub",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton"
  )

  new <- list(
    Package        = "skeleton",
    Version        = "1.0.0",
    Source         = "Gitlab",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton"
  )

  expect_equal(
    renv_record_format_pair(old, new),
    "[1.0.0: GitHub -> Gitlab]"
  )

})

test_that("renv_record_format_short handles missing Version", {

  record <- list(Package = "thispackagedoesnotexist")
  expect_equal(renv_record_format_short(record), "*")

})

test_that("compatible records from pak are handled correctly", {

  lhs <- list(
    Package           = "anytime",
    Version           = "0.3.9",
    Source            = "Repository",
    Depends           = "R (>= 3.2.0)",
    Imports           = "Rcpp (>= 0.12.9)",
    LinkingTo         = c("Rcpp (>= 0.12.9)", "BH"),
    Repository        = "CRAN",
    RemoteType        = "standard",
    RemotePkgRef      = "anytime",
    RemoteRef         = "anytime",
    RemoteRepos       = "https://cran.rstudio.com",
    RemotePkgPlatform = "aarch64-apple-darwin20",
    RemoteSha         = "0.3.9",
    Hash              = "74a64813f17b492da9c6afda6b128e3d"
  )

  rhs <- list(
    Package           = "anytime",
    Version           = "0.3.9",
    Source            = "CRAN",
    Repository        = "CRAN",
    RemoteType        = "standard",
    RemotePkgRef      = "anytime",
    RemoteRef         = "anytime",
    RemoteRepos       = "https://cran.rstudio.com",
    RemotePkgPlatform = "aarch64-apple-darwin20",
    RemoteSha         = "0.3.9",
    Hash              = "74a64813f17b492da9c6afda6b128e3d",
    Requirements      = list()
  )

  change <- renv_lockfile_diff_record(lhs, rhs)
  expect_null(change)

})

test_that("records with NULL versions are treated as crossgrades", {

  # https://github.com/rstudio/renv/issues/2248
  before <- list(Package = "skeleton", Version = NULL)
  after  <- list(Package = "skeleton", Version = "1.0.0")
  expect_equal(renv_lockfile_diff_record(before, after), "crossgrade")

  before <- list(Package = "skeleton", Version = "1.0.0")
  after  <- list(Package = "skeleton", Version = NULL)
  expect_equal(renv_lockfile_diff_record(before, after), "crossgrade")

  before <- list(Package = "skeleton", Version = NULL)
  after  <- list(Package = "skeleton", Version = NULL)
  expect_equal(renv_lockfile_diff_record(before, after), "crossgrade")

})

test_that("'HEAD' refs compare equal to no ref (#2378)", {

  # version 1 lockfiles omit 'HEAD' refs, which git records now use
  # for the default branch
  before <- list(
    Package    = "skeleton",
    Version    = "1.0.0",
    Source     = "git",
    RemoteType = "git",
    RemoteUrl  = "https://github.com/kevinushey/skeleton.git",
    RemoteRef  = "HEAD",
    RemoteSha  = "e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a"
  )

  after <- before
  after$RemoteRef <- NULL
  expect_null(renv_lockfile_diff_record(before, after))

  after$RemoteRef <- "main"
  expect_equal(renv_lockfile_diff_record(before, after), "crossgrade")

})

test_that("pak's cran remotes are considered cranlike", {

  record <- list(
    Package           = "rlang",
    Version           = "1.0.0",
    RemoteType        = "standard",
    RemotePkgRef      = "rlang",
    RemoteRef         = "rlang",
    RemoteRepos       = "https://cloud.R-project.org",
    RemotePkgPlatform = "aarch64-apple-darwin20",
    RemoteSha         = "1.1.3"
  )

  actual <- renv_record_normalize(record)
  expected <- list(Package = "rlang", Version = "1.0.0")
  expect_identical(!!actual, !!expected)

})

test_that("we format github remotes appropriately", {

  record <- list(
    Package        = "skeleton",
    Version        = "1.1.0",
    RemoteType     = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteRef      = "main",
    RemoteSha      = "e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a",
    RemoteSubdir   = "subdir"
  )

  remote <- renv_record_format_remote(record, compact = FALSE)
  expect_equal(remote, "kevinushey/skeleton:subdir@e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

  remote <- renv_record_format_remote(record, compact = TRUE)
  expect_equal(remote, "kevinushey/skeleton:subdir")

  remote <- renv_record_format_remote(record, pak = TRUE)
  expect_equal(remote, "kevinushey/skeleton/subdir@e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

})

test_that("remote hosts are included when formatting", {

  record <- list(
    Package        = "skeleton",
    Version        = "1.1.0",
    RemoteType     = "github",
    RemoteHost     = "github.example.com",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteRef      = "main",
    RemoteSha      = "e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a"
  )

  remote <- renv_record_format_remote(record, compact = FALSE)
  expect_equal(remote, "github@github.example.com::kevinushey/skeleton@e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

  remote <- renv_record_format_remote(record, compact = TRUE)
  expect_equal(remote, "github@github.example.com::kevinushey/skeleton")

  remote <- renv_record_format_remote(record, pak = TRUE)
  expect_equal(remote, "github@github.example.com::kevinushey/skeleton@e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

})

test_that("gitlab remotes are formatted using pkgdepends syntax for pak (#2180)", {

  record <- list(
    Package        = "testpackage",
    Version        = "0.1.0",
    Source         = "GitLab",
    RemoteType     = "gitlab",
    RemoteHost     = "gitlab.example.de",
    RemoteUsername = "bioinf",
    RemoteRepo     = "rlib/testpackage",
    RemoteSubdir   = "",
    RemoteRef      = "main",
    RemoteSha      = "babda05db8146b37a552c77651e2cd084a05ce98"
  )

  # renv's own 'gitlab@host::' syntax is retained for display
  remote <- renv_record_format_remote(record)
  expect_equal(remote, "testpackage=gitlab@gitlab.example.de::bioinf/rlib/testpackage@babda05db8146b37a552c77651e2cd084a05ce98")

  # ... but pak requires the host to be spelled as a URL
  remote <- renv_record_format_remote(record, pak = TRUE)
  expect_equal(remote, "testpackage=gitlab::https://gitlab.example.de/bioinf/rlib/testpackage@babda05db8146b37a552c77651e2cd084a05ce98")

  # the default host is included as well, so that a sub-directory can never
  # be mistaken for the project name
  record$RemoteHost <- NULL
  record$RemoteUsername <- "group"
  record$RemoteRepo <- "repo"
  record$RemoteSubdir <- "testpackage"

  remote <- renv_record_format_remote(record, pak = TRUE)
  expect_equal(remote, "testpackage=gitlab::https://gitlab.com/group/repo/-/testpackage@babda05db8146b37a552c77651e2cd084a05ce98")

  # unversioned remotes use the ref, if any
  remote <- renv_record_format_remote(record, pak = TRUE, versioned = FALSE)
  expect_equal(remote, "testpackage=gitlab::https://gitlab.com/group/repo/-/testpackage@main")

})

test_that("git remotes are formatted using pkgdepends syntax for pak (#2378)", {

  record <- list(
    Package    = "skeleton",
    Version    = "1.1.0",
    Source     = "git",
    RemoteType = "git",
    RemoteUrl  = "https://github.com/kevinushey/skeleton.git",
    RemoteRef  = "main",
    RemoteSha  = "e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a"
  )

  # renv's own syntax is retained for display
  remote <- renv_record_format_remote(record)
  expect_equal(remote, "git::https://github.com/kevinushey/skeleton.git")

  # pak installs the recorded commit
  remote <- renv_record_format_remote(record, pak = TRUE)
  expect_equal(remote, "skeleton=git::https://github.com/kevinushey/skeleton.git@e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

  # unversioned remotes use the ref, if any
  remote <- renv_record_format_remote(record, pak = TRUE, versioned = FALSE)
  expect_equal(remote, "skeleton=git::https://github.com/kevinushey/skeleton.git@main")

  # pkgdepends uses the default branch without a ref, and can't express a
  # pull request refspec
  for (ref in c("HEAD", "pull/1/head:pull/1")) {
    record$RemoteRef <- ref
    remote <- renv_record_format_remote(record, pak = TRUE, versioned = FALSE)
    expect_equal(remote, "skeleton=git::https://github.com/kevinushey/skeleton.git")
  }

  # pak records the remote as requested, which doesn't pin the commit it
  # installed; that pkgref is kept for display, but pak is given the commit
  record$RemotePkgRef <- "git::https://github.com/kevinushey/skeleton.git"
  remote <- renv_record_format_remote(record)
  expect_equal(remote, record$RemotePkgRef)

  remote <- renv_record_format_remote(record, pak = TRUE)
  expect_equal(remote, "skeleton=git::https://github.com/kevinushey/skeleton.git@e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

  # pkgdepends can't install a package from a sub-directory of a git repository
  record$RemoteSubdir <- "pkg"
  expect_error(renv_record_format_remote(record, pak = TRUE), "sub-directory")

})

test_that("renv_record_source infers 'repository' from Repository field", {

  # a record with Source explicitly set uses that
  record <- list(Package = "skeleton", Version = "1.0.0", Source = "GitHub")
  expect_equal(renv_record_source(record), "github")

  # a record with no Source but a Repository field infers 'repository'
  record <- list(Package = "skeleton", Version = "1.0.0", Repository = "CRAN")
  expect_equal(renv_record_source(record), "repository")

  # a record with neither Source nor Repository falls back to 'unknown'
  record <- list(Package = "skeleton", Version = "1.0.0")
  expect_equal(renv_record_source(record), "unknown")

})
