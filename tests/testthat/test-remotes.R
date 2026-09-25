
test_that("we can parse a variety of remotes", {

  remote <- renv_remotes_parse("git@github.com:kevinushey/repo.git:subdir")
  expect_equal(remote$type,   "git")
  expect_equal(remote$host,   "github.com")
  expect_equal(remote$repo,   "kevinushey/repo.git")
  expect_equal(remote$subdir, "subdir")

  remote <- renv_remotes_parse("url::https://github.com/kevinushey/renv.git1/archive/refs/heads/main.zip")
  expect_equal(remote$type,     "url")
  expect_equal(remote$url,      "https://github.com/kevinushey/renv.git1/archive/refs/heads/main.zip")
  expect_equal(remote$protocol, "https")

  # https://github.com/rstudio/renv/issues/1004
  remote <- renv_remotes_parse("git@github.com:abc123/def456.git")
  expect_equal(remote$type,   "git")
  expect_equal(remote$host,   "github.com")
  expect_equal(remote$repo,   "abc123/def456.git")

  # https://github.com/rstudio/renv/issues/667
  remote <- renv_remotes_parse("package=git@github.com:abc123/def456.git")
  expect_equal(remote$package, "package")

  # https://github.com/rstudio/renv/issues/667
  remote <- renv_remotes_parse("git@github.com:abc/def/ghi")
  expect_equal(remote$repo, "abc/def/ghi")

})

test_that("we can parse a variety of remotes", {

  skip_on_cran()
  skip_if_no_github_auth()
  skip_on_os("windows")

  renv_tests_scope()

  # cran latest
  record <- renv_remotes_resolve("breakfast")
  expect_equal(record$Package, "breakfast")
  expect_equal(record$Version, NULL)

  # cran archive
  record <- renv_remotes_resolve("breakfast@0.1.0")
  expect_equal(record$Package, "breakfast")
  expect_equal(record$Version, "0.1.0")

  # github master
  record <- renv_remotes_resolve("kevinushey/skeleton")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.1")
  expect_equal(record$RemoteRef, "master")
  expect_equal(record$RemoteSha, "e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

  # by commit
  record <- renv_remotes_resolve("kevinushey/skeleton@209c4e48e505e545ad7ab915904d983b5ab83b93")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.0")
  expect_equal(record$RemoteSha, "209c4e48e505e545ad7ab915904d983b5ab83b93")

  # by branch
  record <- renv_remotes_resolve("kevinushey/skeleton@feature/version-bump")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.2")
  expect_equal(record$RemoteRef, "feature/version-bump")
  expect_equal(record$RemoteSha, "86b5737411d3c6a6927dfcccd2c15a69284659fe")

  # by PR
  record <- renv_remotes_resolve("kevinushey/skeleton#1")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.2")
  expect_equal(record$RemoteSha, "86b5737411d3c6a6927dfcccd2c15a69284659fe")

  # bitbucket
  record <- renv_remotes_resolve("bitbucket::kevinushey/skeleton")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.1")
  expect_equal(record$RemoteHost, "api.bitbucket.org/2.0")
  expect_equal(record$RemoteRef, "master")
  expect_equal(record$RemoteSha, "958296dbbbf7f1d82f7f5dd1b121c7558604809f")

  # gitlab
  record <- renv_remotes_resolve("gitlab::kevinushey/skeleton")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.1")
  expect_equal(record$RemoteHost, "gitlab.com")
  expect_equal(record$RemoteRef, "master")
  expect_equal(record$RemoteSha, "958296dbbbf7f1d82f7f5dd1b121c7558604809f")

  # git - https
  record <- renv_remotes_resolve("git::https://github.com/kevinushey/renv.git1.git@main")
  expect_equal(record$Package, "renv.git1")
  expect_equal(record$Version, "0.0.0.9000")
  expect_equal(record$RemoteUrl, "https://github.com/kevinushey/renv.git1.git")
  expect_equal(record$RemoteRef, "main")
  expect_match(record$RemoteSha, "^[0-9a-f]{40}$")

  # git + *release
  record <- renv_remotes_resolve("kevinushey/skeleton@*release")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$RemoteRef, "v1.0.1")

  # git + prefix
  record <- renv_remotes_resolve("skeleton=kevinushey/skeleton@*release")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$RemoteRef, "v1.0.1")

  # url
  record <- renv_remotes_resolve("url::https://github.com/kevinushey/renv.git1/archive/refs/heads/main.zip")
  expect_equal(record$Package, "renv.git1")
  expect_equal(record$Version, "0.0.0.9000")
  expect_equal(record$RemoteUrl, "https://github.com/kevinushey/renv.git1/archive/refs/heads/main.zip")
  expect_equal(record$Source, "URL")

  # # git - ssh
  # # this test appears to fail on CI (ssh access to GitHub disallowed?)
  # record <- renv_remotes_resolve("git::git@github.com:kevinushey/renv.git1.git@main")
  # expect_equal(record$Package, "renv.git1")
  # expect_equal(record$Version, "0.0.0.9000")
  # expect_equal(record$RemoteUrl, "git@github.com:kevinushey/renv.git1.git")
  # expect_equal(record$RemoteRef, "main")

  # error
  expect_error(renv_remotes_resolve("can't parse this"))

})

test_that("subdirectories are parsed in remotes", {

  spec <- "gitlab::user/repo:subdir@ref"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec    = spec,
    package = NULL,
    type    = "gitlab",
    host    = NULL,
    user    = "user",
    repo    = "repo",
    subdir  = "subdir",
    pull    = NULL,
    ref     = "ref"
  )

  expect_equal(remote, expected)

})

test_that("custom hosts can be supplied", {

  spec <- "gitlab@localhost::user/repo"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec    = spec,
    package = NULL,
    type    = "gitlab",
    host    = "localhost",
    user    = "user",
    repo    = "repo",
    subdir  = NULL,
    pull    = NULL,
    ref     = NULL
  )

  expect_equal(remote, expected)

})

test_that("paths specified with '.' are treated as local", {

  renv_tests_scope()

  writeLines(con = "DESCRIPTION", c(
    "Type: Package",
    "Package: test",
    "Version: 1.0"
  ))

  record <- renv_remotes_resolve(".")
  expect_equal(record$Package, "test")
  expect_equal(record$Version, "1.0")

})

test_that("packages can be installed from GitLab groups", {

  # test parsing of spec
  spec <- "gitlab::renv-group/renv-subgroup/subpackage"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec    = spec,
    package = NULL,
    type    = "gitlab",
    host    = NULL,
    user    = "renv-group",
    repo    = "renv-subgroup/subpackage",
    subdir  = NULL,
    pull    = NULL,
    ref     = NULL
  )

  expect_equal(remote, expected)

  # test installation
  skip_slow()
  renv_tests_scope()
  install(spec)
  expect_true(renv_package_installed("subpackage"))

})

test_that("remote specs referencing packages in sub-sub-directories are parsed correctly", {

  spec <- "github::user/repo/subdir/subsubdir"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec    = spec,
    package = NULL,
    type    = "github",
    host    = NULL,
    user    = "user",
    repo    = "repo",
    subdir  = "subdir/subsubdir",
    pull    = NULL,
    ref     = NULL
  )

  expect_equal(remote, expected)

})

test_that("we can parse remotes containing multiple '@'s", {

  spec <- "user/repo@ref-has-@-inside"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec    = spec,
    package = NULL,
    type    = "github",
    host    = NULL,
    user    = "user",
    repo    = "repo",
    subdir  = NULL,
    pull    = NULL,
    ref     = "ref-has-@-inside"
  )

  expect_equal(remote, expected)

})

test_that("git remotes record the commit they were resolved from", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  project <- renv_tests_scope()
  init()

  # a local git repository holding 'bread' 0.5.0 on the default branch,
  # and 1.0.0 on a 'release' branch
  repo <- renv_scope_tempfile("renv-git-")
  ensure_directory(repo)

  local({

    renv_scope_wd(repo)
    renv_system_exec("git", c("init", "--quiet"), action = "git init")
    renv_system_exec("git", c("config", "user.name", shQuote("User Name")), action = "git config")
    renv_system_exec("git", c("config", "user.email", shQuote("user@example.com")), action = "git config")

    # restore fetches by sha; a local repository needs to be told to allow it
    renv_system_exec("git", c("config", "uploadpack.allowAnySHA1InWant", "true"), action = "git config")

    writeLines(c("Package: bread", "Type: Package", "Version: 0.5.0"), con = "DESCRIPTION")
    writeLines("", con = "NAMESPACE")
    renv_system_exec("git", c("add", "-A"), action = "git add")
    renv_system_exec("git", c("commit", "--quiet", "-m", "0.5.0"), action = "git commit")

    renv_system_exec("git", c("checkout", "--quiet", "-b", "release"), action = "git checkout")
    writeLines(c("Package: bread", "Type: Package", "Version: 1.0.0"), con = "DESCRIPTION")
    renv_system_exec("git", c("commit", "--quiet", "-am", "1.0.0"), action = "git commit")
    renv_system_exec("git", c("checkout", "--quiet", "-"), action = "git checkout")

  })

  # use a file URL, so that the repository path is not mistaken for a local
  # package source when the package is later retrieved
  url <- paste0("file://", renv_path_normalize(repo))
  shas <- list(
    head    = renv_git_sha(repo),
    release = local({
      renv_scope_wd(repo)
      renv_system_exec("git", c("rev-parse", "release"), action = "git rev-parse")
    })
  )

  remote <- list(url = url, repo = "bread")

  # no ref: the default branch is resolved, and its commit recorded
  record <- renv_remotes_resolve_git(remote)
  expect_equal(record$Version, "0.5.0")
  expect_equal(record$RemoteRef, "HEAD")
  expect_equal(record$RemoteSha, shas$head)

  # an explicit ref resolves to that ref's commit
  record <- renv_remotes_resolve_git(c(remote, ref = "release"))
  expect_equal(record$Version, "1.0.0")
  expect_equal(record$RemoteRef, "release")
  expect_equal(record$RemoteSha, shas$release)

  # the recorded ref can be re-resolved to its current commit
  expect_equal(renv_remotes_resolve_git_sha_ref(record), shas$release)

  # the sha is written to the installed package, and captured in the lockfile;
  # installing re-uses the clone made while resolving, rather than cloning again
  local({
    renv_scope_binding(
      envir = asNamespace("renv"),
      symbol = "renv_retrieve_git_impl",
      replacement = function(record, path) {
        stop("unexpected clone of '", record$RemoteUrl, "'")
      }
    )
    install(list(record))
  })
  expect_null(the$git_clones[[renv_git_clone_key(record)]])
  desc <- renv_description_read(package = "bread")
  expect_equal(desc$RemoteType, "git")
  expect_equal(desc$RemoteSha, shas$release)

  writeLines("library(bread)", con = file.path(project, "dependencies.R"))
  snapshot()
  lockfile <- renv_lockfile_read(file.path(project, "renv.lock"))
  expect_equal(lockfile$Packages$bread$Source, "git")
  expect_equal(lockfile$Packages$bread$RemoteSha, shas$release)

  # restore retrieves the recorded commit even after the ref moves on
  local({
    renv_scope_wd(repo)
    renv_system_exec("git", c("checkout", "--quiet", "release"), action = "git checkout")
    writeLines(c("Package: bread", "Type: Package", "Version: 2.0.0"), con = "DESCRIPTION")
    renv_system_exec("git", c("commit", "--quiet", "-am", "2.0.0"), action = "git commit")
  })

  remove("bread")
  restore()
  desc <- renv_description_read(package = "bread")
  expect_equal(desc$Version, "1.0.0")
  expect_equal(desc$RemoteSha, shas$release)

})
