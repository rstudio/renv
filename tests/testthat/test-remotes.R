
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

test_that("git remotes are pinned to the commit they were installed from", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  # use a separate cache, since the versions of 'bread' installed here would
  # otherwise shadow those from the test repositories in later tests
  project <- renv_tests_scope(isolated = TRUE)
  init()

  remote <- renv_tests_git_remote()
  shas <- remote$shas

  # count the clones made, and check that none are left behind
  clones <- renv_tests_git_scope_clones()
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
  # clone made to resolve a remote to read its DESCRIPTION, and to install it
  clones$count <- 0L
  local({
    renv_tests_git_scope_spec("bread@release", list(url = remote$url, repo = "bread", ref = "release"))
    install("bread@release")
  })
  expect_equal(clones$count, 1L)
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

  # NOTE: git marks its object files read-only, hence 'force'
  unlink(paths[["bread"]], recursive = TRUE, force = TRUE)

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
  expect_setequal(list.files(tempdir(), pattern = "^renv-git-"), tmpfiles)

  # update() installs an update using the clone made to find it
  clones$count <- 0L
  update("bread", prompt = FALSE)
  expect_equal(clones$count, 1L)
  expect_null(the$git_clones)
  expect_setequal(list.files(tempdir(), pattern = "^renv-git-"), tmpfiles)

  desc <- renv_description_read(package = "bread")
  expect_equal(desc$Version, "2.0.0")
  expect_equal(desc$RemoteSha, latest)

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
  clones$count <- 0L
  restore(rebuild = TRUE)
  expect_equal(clones$count, 1L)
  expect_null(the$git_clones)
  expect_setequal(list.files(tempdir(), pattern = "^renv-git-"), tmpfiles)

  desc <- renv_description_read(package = "bread")
  expect_equal(desc$Version, "1.0.0")
  expect_equal(desc$RemoteSha, shas$release)

  # records written by older versions of renv have no sha; restoring one
  # records the commit that was installed
  lockfile$Packages$bread$RemoteSha <- NULL
  renv_lockfile_write(lockfile, file = file.path(project, "renv.lock"))

  remove("bread")
  restore(rebuild = TRUE)

  desc <- renv_description_read(package = "bread")
  expect_equal(desc$Version, "2.0.0")
  expect_equal(desc$RemoteSha, latest)

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

    # also add refs that 'ls-remote' lists ahead of the ones 'git fetch' uses:
    # a branch whose name ends with 'release', and a 'stable' branch and tag
    renv_system_exec("git", c("branch", "feature/release", shas$head), action = "git branch")
    renv_system_exec("git", c("branch", "stable", shas$head), action = "git branch")
    renv_system_exec("git", c("tag", "stable", shas$release), action = "git tag")
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

  # a record without a sha may not be current, but is only reported as out of
  # date if a newer version is available
  record$RemoteRef <- "release"
  expect_length(renv_update_find(list(bread = record)), 0L)

  record$Version <- "0.5.0"
  updates <- renv_update_find(list(bread = record))
  expect_equal(updates$bread$Version, "1.0.0")
  expect_equal(updates$bread$RemoteSha, shas$release)

  record$Version <- "1.0.0"
  record$RemoteRef <- NULL

  # refs resolve to the commit that 'git fetch' checks out, which prefers an
  # exact match to one that only ends with the ref, and tags to branches
  record$RemoteRef <- "release"
  expect_equal(renv_remotes_resolve_git_sha_ref(record), shas$release)

  resolved <- renv_remotes_resolve_git(list(url = remote$url, repo = "bread", ref = "stable"))
  expect_equal(resolved$RemoteSha, shas$release)
  expect_equal(renv_remotes_resolve_git_sha_ref(resolved), shas$release)

  # annotated tags resolve to the commit they point at, followed by the tag
  # object itself, and so report no update for the installed commit
  record$RemoteRef <- "v1.0.0"
  record$RemoteSha <- shas$release
  tagged <- renv_remotes_resolve_git_sha_ref(record)
  expect_length(tagged, 2L)
  expect_equal(tagged[[1L]], shas$release)
  expect_null(renv_update_find_git_impl(record))

  # remotes records the sha of the tag object instead, which is also current
  record$RemoteSha <- tagged[[2L]]
  expect_null(renv_update_find_git_impl(record))

  # pull requests are recorded as refspecs
  resolved <- renv_remotes_resolve_git(list(url = remote$url, repo = "bread", pull = "1"))
  expect_equal(resolved$Version, "1.0.0")
  expect_equal(resolved$RemoteRef, "pull/1/head:pull/1")
  expect_equal(resolved$RemoteSha, shas$release)
  expect_equal(renv_remotes_resolve_git_sha_ref(resolved), shas$release)

  # a commit isn't a ref, and so can't be resolved; nor can it be updated
  record$RemoteRef <- shas$release
  expect_length(renv_remotes_resolve_git_sha_ref(record), 0L)
  expect_null(renv_update_find_git_impl(record))

  # but a ref that no longer exists (e.g. a deleted branch) is an error
  record$RemoteRef <- "deleted"
  expect_error(renv_update_find_git_impl(record), "ref 'deleted' was not found")

  # refs are passed through the shell, and may contain shell metacharacters
  local({
    renv_scope_wd(remote$repo)
    renv_system_exec("git", c("branch", shQuote("fix(ci)"), shas$release), action = "git branch")
  })

  record$RemoteRef <- "fix(ci)"
  expect_equal(renv_remotes_resolve_git_sha_ref(record), shas$release)

})

test_that("clones made while checking for git updates are kept for install", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  renv_tests_scope()
  remote <- renv_tests_git_remote()
  shas <- remote$shas

  # check two records, so that (except on Windows) each is checked in a
  # forked process, whose changes to the clone cache would otherwise be lost
  renv_scope_options(renv.config.updates.parallel = 2L)

  record <- list(
    Package    = "bread",
    Version    = "0.5.0",
    Source     = "git",
    RemoteType = "git",
    RemoteUrl  = remote$url,
    RemoteRef  = "release",
    RemoteSha  = shas$head
  )

  records <- list(record, overlay(record, list(RemoteRef = "HEAD", RemoteSha = shas$release)))

  paths <- local({

    clones <- renv_scope_git_clones()
    updates <- renv_update_find_git(records)
    expect_equal(map_chr(updates, `[[`, "RemoteSha"), c(bread = shas$release, bread = shas$head))

    # the clones made by the forked processes are adopted by this one
    expect_length(clones$paths, 2L)
    expect_true(all(dir.exists(clones$paths)))
    expect_equal(renv_git_clone(updates[[1L]]), clones$keys[[renv_git_clone_key(updates[[1L]])]])

    clones$paths

  })

  # and are removed along with the others
  expect_false(any(dir.exists(paths)))

})

test_that("restore() finds a pinned commit deep within the history of a ref", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  renv_tests_scope()
  remote <- renv_tests_git_remote()
  shas <- remote$shas

  # move the release commit beyond the recent history fetched at first
  renv_tests_git_extend(remote$repo, "release", 150L)

  # and add a commit which no ref points at, e.g. one that was force-pushed away
  local({
    renv_scope_wd(remote$repo)
    renv_system_exec("git", c("checkout", "--quiet", "-b", "doomed"), action = "git checkout")
  })
  doomed <- renv_tests_git_commit(remote$repo, "3.0.0")
  local({
    renv_scope_wd(remote$repo)
    renv_system_exec("git", c("checkout", "--quiet", "-"), action = "git checkout")
    renv_system_exec("git", c("branch", "--quiet", "-D", "doomed"), action = "git branch")
  })

  # make the server refuse to serve a commit by its sha
  renv_scope_envvars(
    GIT_CONFIG_COUNT   = "1",
    GIT_CONFIG_KEY_0   = "protocol.version",
    GIT_CONFIG_VALUE_0 = "0"
  )

  record <- list(
    Package    = "bread",
    Version    = "1.0.0",
    Source     = "git",
    RemoteType = "git",
    RemoteUrl  = remote$url,
    RemoteRef  = "release",
    RemoteSha  = shas$release
  )

  path <- renv_scope_tempfile("renv-clone-")
  renv_retrieve_git_impl(record, path)
  expect_equal(renv_git_sha(path), shas$release)
  expect_false(file.exists(file.path(path, ".git/shallow")))

  # a commit that isn't part of the ref's history is reported as such
  record$RemoteSha <- doomed
  path <- renv_scope_tempfile("renv-clone-")
  expect_error(
    renv_retrieve_git_impl(record, path),
    sprintf("commit '%s' was not found in the history of 'release'", doomed)
  )

})
