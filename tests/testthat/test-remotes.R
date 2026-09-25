
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

# write a DESCRIPTION for 'bread' at the requested version, and commit it
renv_tests_git_commit <- function(repo, version) {

  renv_scope_wd(repo)

  desc <- c("Package: bread", "Type: Package", paste("Version:", version))
  writeLines(desc, con = "DESCRIPTION")
  writeLines("", con = "NAMESPACE")

  renv_system_exec("git", c("add", "-A"), action = "git add")
  renv_system_exec("git", c("commit", "--quiet", "-m", version), action = "git commit")
  renv_system_exec("git", c("rev-parse", "HEAD"), action = "git rev-parse")

}

# a local git repository holding 'bread' 0.5.0 on its default branch, and
# 1.0.0 on a 'release' branch
renv_tests_git_remote <- function(scope = parent.frame()) {

  repo <- renv_scope_tempfile("renv-repo-", scope = scope)
  ensure_directory(repo)
  renv_scope_wd(repo)

  renv_system_exec("git", c("init", "--quiet"), action = "git init")
  renv_system_exec("git", c("config", "user.name", shQuote("User Name")), action = "git config")
  renv_system_exec("git", c("config", "user.email", shQuote("user@example.com")), action = "git config")

  head <- renv_tests_git_commit(repo, "0.5.0")

  renv_system_exec("git", c("checkout", "--quiet", "-b", "release"), action = "git checkout")
  release <- renv_tests_git_commit(repo, "1.0.0")
  renv_system_exec("git", c("checkout", "--quiet", "-"), action = "git checkout")

  # use a file URL, so that the repository path is not mistaken for a local
  # package source when the package is later retrieved
  list(
    repo = repo,
    url  = paste0("file://", renv_path_normalize(repo)),
    shas = list(head = head, release = release)
  )

}

test_that("git remotes are pinned to the commit they were installed from", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  project <- renv_tests_scope()
  init()

  remote <- renv_tests_git_remote()
  shas <- remote$shas

  # count the clones made, and check that none are left behind
  clones <- 0L
  impl <- renv_retrieve_git_impl
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = "renv_retrieve_git_impl",
    replacement = function(record, path) {
      clones <<- clones + 1L
      impl(record, path)
    }
  )

  tmpfiles <- list.files(tempdir(), pattern = "^renv-git-")

  # resolving a remote with no ref records the commit of the default branch;
  # the clone made to do so is removed again, as it's not part of an install
  record <- renv_remotes_resolve_git(list(url = remote$url, repo = "bread"))
  expect_equal(record$Version, "0.5.0")
  expect_equal(record$RemoteRef, "HEAD")
  expect_equal(record$RemoteSha, shas$head)
  expect_null(the$git_clones)
  expect_setequal(list.files(tempdir(), pattern = "^renv-git-"), tmpfiles)

  # install() resolves remotes as part of the install, and so re-uses the
  # clone made to resolve a remote to read its DESCRIPTION, and to install it;
  # its remote parser doesn't accept file URLs, so we do the same here
  clones <- 0L
  local({
    renv_scope_git_clones()
    record <- renv_remotes_resolve_git(list(url = remote$url, repo = "bread", ref = "release"))
    install(list(record))
  })
  expect_equal(clones, 1L)
  expect_null(the$git_clones)
  expect_setequal(list.files(tempdir(), pattern = "^renv-git-"), tmpfiles)

  desc <- renv_description_read(package = "bread")
  expect_equal(desc$RemoteType, "git")
  expect_equal(desc$RemoteRef, "release")
  expect_equal(desc$RemoteSha, shas$release)

  # the commit is captured in the lockfile
  writeLines("library(bread)", con = file.path(project, "dependencies.R"))
  snapshot()
  lockfile <- renv_lockfile_read(file.path(project, "renv.lock"))
  record <- lockfile$Packages$bread
  expect_equal(record$Source, "git")
  expect_equal(record$RemoteSha, shas$release)

  # retrieve() hands the clone back to its caller, so it isn't removed
  destdir <- renv_scope_tempfile("renv-destdir-")
  paths <- retrieve("bread", lockfile = file.path(project, "renv.lock"), destdir = destdir)
  expect_true(file.exists(file.path(paths[["bread"]], "DESCRIPTION")))
  unlink(paths[["bread"]], recursive = TRUE)

  # update() sees no update until the recorded ref moves on
  expect_length(renv_update_find(list(bread = record)), 0L)

  local({
    renv_scope_wd(remote$repo)
    renv_system_exec("git", c("checkout", "--quiet", "release"), action = "git checkout")
  })
  latest <- renv_tests_git_commit(remote$repo, "2.0.0")

  updates <- renv_update_find(list(bread = record))
  expect_equal(updates$bread$Version, "2.0.0")
  expect_equal(updates$bread$RemoteSha, latest)

  # restore retrieves the recorded commit, even though the ref has moved on.
  # git's original wire protocol refuses to serve a commit that no ref points
  # at (as do some servers), so restore needs to fetch the ref's history
  renv_scope_envvars(
    GIT_CONFIG_COUNT   = "1",
    GIT_CONFIG_KEY_0   = "protocol.version",
    GIT_CONFIG_VALUE_0 = "0"
  )

  # (rebuild, so that the package isn't just restored from the cache)
  remove("bread")
  clones <- 0L
  restore(rebuild = TRUE)
  expect_equal(clones, 1L)
  expect_null(the$git_clones)
  expect_setequal(list.files(tempdir(), pattern = "^renv-git-"), tmpfiles)

  desc <- renv_description_read(package = "bread")
  expect_equal(desc$Version, "1.0.0")
  expect_equal(desc$RemoteSha, shas$release)

})

test_that("git refs are resolved to the commits they point at", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  renv_tests_scope()
  remote <- renv_tests_git_remote()
  shas <- remote$shas

  # point an annotated tag, and a pull request ref (like those GitHub
  # provides), at the release commit
  local({
    renv_scope_wd(remote$repo)
    renv_system_exec("git", c("tag", "-a", "v1.0.0", "-m", "v1.0.0", "release"), action = "git tag")
    renv_system_exec("git", c("update-ref", "refs/pull/1/head", shas$release), action = "git update-ref")
  })

  record <- list(
    Package    = "bread",
    Version    = "1.0.0",
    Source     = "git",
    RemoteType = "git",
    RemoteUrl  = remote$url
  )

  # records without a ref use the default branch
  expect_equal(renv_remotes_resolve_git_sha_ref(record), shas$head)

  # annotated tags resolve to the commit they point at, rather than to the
  # tag object itself, and so report no update for the installed commit
  record$RemoteRef <- "v1.0.0"
  record$RemoteSha <- shas$release
  expect_equal(renv_remotes_resolve_git_sha_ref(record), shas$release)
  expect_null(renv_update_find_git_impl(record))

  # pull requests are recorded as refspecs
  resolved <- renv_remotes_resolve_git(list(url = remote$url, repo = "bread", pull = "1"))
  expect_equal(resolved$Version, "1.0.0")
  expect_equal(resolved$RemoteRef, "pull/1/head:pull/1")
  expect_equal(resolved$RemoteSha, shas$release)
  expect_equal(renv_remotes_resolve_git_sha_ref(resolved), shas$release)

  # a commit isn't a ref, and so can't be resolved; nor can it be updated
  record$RemoteRef <- shas$release
  expect_equal(renv_remotes_resolve_git_sha_ref(record), "")
  expect_null(renv_update_find_git_impl(record))

})
