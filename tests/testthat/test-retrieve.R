
test_that("we can retrieve packages from CRAN", {

  skip_slow()

  renv_tests_scope()

  record <- list(
    Package = "oatmeal",
    Version = "1.0.0",
    Source  = "CRAN"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from the CRAN archive", {

  skip_slow()

  renv_tests_scope()

  record <- list(
    Package = "bread",
    Version = "0.1.0",
    Source  = "CRAN"
  )

  renv_test_retrieve(record)

})

test_that("packages with an unknown source are retrieved from CRAN", {

  skip_slow()

  renv_tests_scope()

  record <- list(
    Package = "bread",
    Version = "0.1.0",
    Source  = "unknown"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from Bitbucket", {

  skip_on_cran()
  skip("unreliable test")

  record <- list(
    Package        = "skeleton",
    Source         = "bitbucket",
    RemoteRepo     = "skeleton",
    RemoteUsername = "kevinushey",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})


test_that("we can retrieve packages from git", {

  skip_on_cran()
  skip("unreliable test")

  record <- list(
    Package   = "skeleton",
    Source    = "git",
    RemoteUrl = "git://github.com/kevinushey/skeleton.git",
    RemoteSha = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages with git dependencies", {
  skip_on_cran()
  skip_slow()

  # GitHub doesn't like ssh (used as remote field in renv.git1)
  skip_on_ci()

  record <- list(
    Package   = "renv.git1",
    Source    = "git",
    RemoteUrl = "https://github.com/kevinushey/renv.git1",
    RemoteRef = "main"
  )

  renv_test_retrieve(record)
})


test_that("we can retrieve packages from GitHub", {

  skip_slow()

  record <- list(
    Package        = "skeleton",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from GitHub (in a sub-directory)", {

  skip_slow()

  record <- list(
    Package        = "subdir",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "subdir",
    RemoteSubdir   = "subdir",
    RemoteSha      = "100373b23c8adae1da4e4d6995402d40e9227cfb"
  )

  renv_test_retrieve(record)

})


test_that("we can retrieve packages from GitLab", {

  skip_slow()

  record <- list(
    Package        = "skeleton",
    Source         = "gitlab",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages with URLs", {
  skip_slow()
  url <- "https://api.github.com/repos/kevinushey/skeleton/tarball"
  record <- renv_remotes_resolve(url)
  renv_test_retrieve(record)
})

test_that("we can retrieve packages from URL sources", {

  skip_slow()

  renv_tests_scope()
  renv_scope_local()

  record <- list(
    Package    = "skeleton",
    Version    = "1.0.1",
    Source     = "URL",
    RemoteType = "url",
    RemoteUrl  = "https://api.github.com/repos/kevinushey/skeleton/tarball"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from local sources", {

  renv_tests_scope()
  renv_scope_local()

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "local"
  )

  renv_test_retrieve(record)

})

test_that("compatible local sources are preferred when available", {

  renv_tests_scope()
  renv_scope_local()

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "CRAN"
  )

  renv_test_retrieve(record)

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "unknown"
  )

  renv_test_retrieve(record)

})

test_that("an explicitly-provided local source path can be used", {

  renv_tests_scope()
  renv_scope_local()

  source <- renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")
  renv_scope_wd(tempdir())

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = source
  )

  renv_test_retrieve(record)

})



test_that("explicit path to binary packages work", {

  skip_if_not(renv_platform_macos())

  renv_tests_scope()
  renv_scope_local()

  # make sure we have a binary package in the cellar to test with
  srcpath <- renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")
  binpath <- renv_tests_path("local/skeleton/skeleton_1.0.1.tgz")
  defer(unlink(binpath))

  # create the binary
  local({
    renv_scope_wd(dirname(srcpath))
    args <- c("CMD", "INSTALL", "--build", basename(srcpath))
    renv_system_exec(R(), args)
  })

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = binpath
  )

  renv_test_retrieve(record)

})

test_that("remotes::install_local() records are handled", {

  renv_scope_envvars(RENV_PATHS_LOCAL = NULL)

  record <- list(
    Package    = "skeleton",
    Version    = "1.0.1",
    Source     = "local",
    RemoteUrl  = renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from GitHub", {

  skip_slow()

  record <- list(
    Package        = "skeleton",
    Source         = "github",
    RemoteHost     = "https://api.github.com",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from R repositories", {

  skip_on_cran()
  renv_tests_scope()

  record <- list(
    Package    = "oatmeal",
    Version    = "1.0.0",
    Source     = "Repository",
    Repository = "CRAN"
  )

  renv_test_retrieve(record)

})

test_that("failed binary downloads are quiet when a source fallback succeeds", {

  skip_on_cran()
  skip_if(identical(.Platform$pkgType, "source"))

  renv_tests_scope(isolated = TRUE)

  # create a repository containing a source tarball for 'bread'
  repopath <- renv_scope_tempfile("renv-repos-")
  srccontrib <- file.path(repopath, "src/contrib")
  ensure_directory(srccontrib)

  tarball <- file.path(srccontrib, "bread_1.0.0.tar.gz")
  local({
    renv_scope_wd(renv_tests_path("packages"))
    tar(tarball, "bread", compression = "gzip", tar = "internal")
  })
  write_PACKAGES(srccontrib, type = "source")

  # advertise a binary package without actually providing its archive
  bincontrib <- paste0(repopath, contrib.url("", type = .Platform$pkgType))
  ensure_directory(bincontrib)
  file.copy(file.path(srccontrib, "PACKAGES"), bincontrib)

  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  renv_scope_options(
    pkgType = "both",
    repos = c(CRAN = sprintf(fmt, repopath)),
    renv.verbose = TRUE
  )

  record <- list(
    Package = "bread",
    Version = "1.0.0",
    Source  = "Repository"
  )

  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)
  renv_scope_restore(
    project = getwd(),
    library = library,
    records = list(bread = record),
    packages = "bread",
    recursive = TRUE
  )

  output <- capture.output(status <- renv_retrieve_repos(record))

  # the failed binary download should be suppressed,
  # while the successful source download is still reported
  expect_true(status)
  expect_false(any(grepl("ERROR", output, fixed = TRUE)))
  expect_true(any(grepl("OK", output, fixed = TRUE)))

})

test_that("tagged archive records try their resolved URL first", {

  renv_tests_scope()
  renv_scope_options(pkgType = "source")

  record <- renv_record_tag_archive(
    record = list(
      Package = "bread",
      Version = "1.0.0",
      Source = "Repository"
    ),
    type = "source",
    url = "https://example.com/archive/bread",
    name = "EXPLICIT"
  )

  called <- FALSE
  local_mocked_bindings(
    renv_retrieve_repos_impl = function(record, ...) {
      called <<- TRUE
      TRUE
    }
  )

  expect_true(renv_retrieve_repos(record))
  expect_true(called)

})

test_that("tagged archive records fall back to later repositories", {

  renv_tests_scope(isolated = TRUE)

  tarball <- file.path(renv_tests_repopath(), "src/contrib/bread_1.0.0.tar.gz")
  first <- renv_tests_archive_repo(package = "bread", version = "1.0.0")
  second <- renv_tests_archive_repo(
    package = "bread",
    version = "1.0.0",
    tarball = tarball
  )

  repos <- c(FIRST = first$url, SECOND = second$url)
  renv_scope_options(
    pkgType = "source",
    repos = repos,
    renv.config.crandb.enabled = FALSE,
    renv.install.allowArchivedPackages = TRUE
  )

  # the first archive index advertises the package but its tarball is absent;
  # the tagged URL is useful to the parallel downloader, but sequential
  # retrieval must still continue to the second repository
  record <- renv_available_packages_latest_archive(
    package = "bread",
    type = "source",
    repos = repos
  )
  expect_equal(record$Repository, "FIRST")
  expect_true(renv_record_archived(record))

  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)
  renv_scope_restore(
    project = getwd(),
    library = library,
    records = list(bread = record),
    packages = "bread",
    recursive = TRUE
  )

  expect_true(renv_retrieve_repos(record))

})

test_that("download errors are reported when all retrieval candidates fail", {

  skip_on_cran()

  renv_tests_scope(isolated = TRUE)

  # create a repository advertising a package whose archive is missing
  repopath <- renv_scope_tempfile("renv-repos-")
  srccontrib <- file.path(repopath, "src/contrib")
  ensure_directory(srccontrib)

  tarball <- file.path(srccontrib, "bread_1.0.0.tar.gz")
  local({
    renv_scope_wd(renv_tests_path("packages"))
    tar(tarball, "bread", compression = "gzip", tar = "internal")
  })
  write_PACKAGES(srccontrib, type = "source")
  unlink(tarball)

  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  renv_scope_options(
    pkgType = "source",
    repos = c(CRAN = sprintf(fmt, repopath)),
    renv.verbose = TRUE
  )

  record <- list(
    Package = "bread",
    Version = "1.0.0",
    Source  = "Repository"
  )

  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)
  renv_scope_restore(
    project = getwd(),
    library = library,
    records = list(bread = record),
    packages = "bread",
    recursive = TRUE
  )

  # NOTE: download errors are reported via 'warning(<error>)', so they are
  # signaled with their original condition class -- collect and muffle them
  # here so they don't unwind to an enclosing tryCatch()
  warnings <- list()
  output <- capture.output(
    status <- catch(
      withCallingHandlers(
        renv_retrieve_repos(record),
        condition = function(cnd) {
          if (inherits(cnd, "error") && !is.null(findRestart("muffleWarning"))) {
            warnings[[length(warnings) + 1L]] <<- cnd
            invokeRestart("muffleWarning")
          }
        }
      )
    )
  )

  expect_s3_class(status, "error")
  expect_match(conditionMessage(status), "failed to retrieve")

  # the failed download transcript is replayed
  expect_true(any(grepl("ERROR", output, fixed = TRUE)))

  # each failure is reported only once
  messages <- map_chr(warnings, conditionMessage)
  expect_false(any(duplicated(messages)))

})

test_that("we can retrieve files using file URIs", {

  skip_on_cran()
  renv_tests_scope()

  source <- file.path(getwd(), "source")
  target <- file.path(getwd(), "target")

  writeLines("Hello, world!", con = source)

  # plain 'file:' URI with no authority
  uri <- paste("file:", source, sep = "")
  download(uri, destfile = target)
  expect_equal(readLines(target), "Hello, world!")
  unlink(target)

  # file URI using empty authority
  prefix <- if (renv_platform_windows()) "file:///" else "file://"
  uri <- paste(prefix, source, sep = "")
  download(uri, destfile = target)
  expect_equal(readLines(target), "Hello, world!")
  unlink(target)

})

test_that("records with RemoteSha successfully retrieved from archives", {

  renv_tests_scope()

  record <- list(
    Package   = "bread",
    Version   = "0.1.0",
    Source    = "Repository",
    RemoteSha = "oops"
  )

  renv_test_retrieve(record)

})

test_that("we respect the default branch for gitlab repositories", {
  skip_on_cran()
  remote <- renv_remotes_resolve("gitlab::kevinushey/main")
  expect_equal(remote$RemoteRef, "main")
})

test_that("renv can retrieve the latest release associated with a project", {
  skip_if_no_github_auth()

  remote <- renv_remotes_resolve("rstudio/keras@*release")
  expect_true(is.list(remote))
})

test_that("retrieve handles local sources", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_options(renv.config.cache.enabled = FALSE)

  record <- list(
    Package   = "bread",
    Version   = "1.0.0",
    Source    = "bread_1.0.0.tar.gz"
  )

  expect_error(renv_test_retrieve(record))

  # call download.packages once to get URL
  url <- download.packages("bread", destdir = getwd())
  if (!file.exists(record$Source))
    file.copy(url[1, 2], record$Source)

  renv_test_retrieve(record)

})

test_that("explicitly local packages fall back to repository sources", {

  renv_tests_scope()
  renv_scope_options(renv.config.cache.enabled = FALSE)

  record <- list(
    Package = "bread",
    Version = "1.0.0",
    Source  = "Local"
  )

  renv_test_retrieve(record)

})

test_that("we can use retrieve() to download packages without installing", {
  project <- renv_tests_scope()
  init()

  result <- retrieve(packages = "breakfast")
  expect_false(renv_package_installed("breakfast"))
  expect_contains(names(result), "breakfast")
  expect_contains(names(result), "bread")

  result <- retrieve(packages = "bread", destdir = ".")
  expect_equal(result, c(bread = "./bread_1.0.0.tar.gz"))

  install("bread")
  result <- retrieve(packages = "bread", destdir = ".")
  expect_equal(result, c(bread = "./bread_1.0.0.tar.gz"))

})

test_that("renv_retrieve_libpaths_impl reuses a compatible installed package", {

  # https://github.com/rstudio/renv/issues/2288 — the built-version check used
  # to compare a version string to TRUE, which always failed, so the user /
  # site library shortcut never fired and renv would needlessly download.
  renv_tests_scope()

  # install bread 1.0.0 into a temporary "user library"
  userlib <- renv_scope_tempfile("renv-userlib-")
  ensure_directory(userlib)
  install("bread", library = userlib)

  # set up restore state so renv_retrieve_successful() has somewhere to record
  templib <- renv_scope_tempfile("renv-library-")
  ensure_directory(templib)
  renv_scope_libpaths(c(templib, .libPaths()))

  record <- list(
    Package    = "bread",
    Version    = "1.0.0",
    Source     = "Repository",
    Repository = "CRAN"
  )

  renv_scope_restore(
    project   = getwd(),
    library   = templib,
    records   = list(bread = record),
    packages  = "bread",
    recursive = TRUE
  )

  expect_true(renv_retrieve_libpaths_impl(record, userlib))

  state <- renv_restore_state()
  expect_true(state$install$contains("bread"))
  expect_equal(state$install$get("bread")$Path, file.path(userlib, "bread"))

})

test_that("renv_retrieve_libpaths_impl rejects packages built for a different R version", {

  renv_tests_scope()

  # the file-backed DESCRIPTION cache would otherwise return the original
  # Built field after we mutate the file below
  renv_scope_options(renv.config.filebacked.cache = FALSE)

  userlib <- renv_scope_tempfile("renv-userlib-")
  ensure_directory(userlib)
  install("bread", library = userlib)

  # rewrite the Built field to claim an R version we definitely aren't using
  descpath <- file.path(userlib, "bread", "DESCRIPTION")
  text <- readLines(descpath)
  text <- sub("^Built: R [0-9.]+", "Built: R 0.0.0", text)
  writeLines(text, descpath)

  templib <- renv_scope_tempfile("renv-library-")
  ensure_directory(templib)
  renv_scope_libpaths(c(templib, .libPaths()))

  record <- list(
    Package    = "bread",
    Version    = "1.0.0",
    Source     = "Repository",
    Repository = "CRAN"
  )

  renv_scope_restore(
    project   = getwd(),
    library   = templib,
    records   = list(bread = record),
    packages  = "bread",
    recursive = TRUE
  )

  expect_false(renv_retrieve_libpaths_impl(record, userlib))

})

# simulate a Bioconductor package whose recorded version has been superseded,
# as happens routinely with the devel branch of Bioconductor. we create:
#
# - a git repository holding versions 1.0.0 and then 1.0.1 of the package;
# - a package repository providing only version 1.0.1, with no archive;
# - a record for version 1.0.0, as Bioconductor would have stamped it.
#
# https://github.com/rstudio/renv/issues/2370
renv_tests_bioconductor_superseded <- function(scope = parent.frame()) {

  root <- renv_scope_tempfile("renv-bioc-", scope = scope)
  repo <- file.path(root, "biocpkg")
  ensure_directory(repo)
  renv_scope_wd(repo)

  renv_system_exec("git", c("init", "--quiet"), action = "git init")
  renv_system_exec("git", c("checkout", "--quiet", "-b", "devel"), action = "git checkout")
  renv_system_exec("git", c("config", "user.name", shQuote("User Name")), action = "git config")
  renv_system_exec("git", c("config", "user.email", shQuote("user@example.com")), action = "git config")

  versions <- c("1.0.0", "1.0.1")
  dates <- c("2020-01-01T12:00:00", "2020-02-01T12:00:00")
  shas <- character()

  for (i in seq_along(versions)) {

    desc <- c(
      "Package: biocpkg",
      "Type: Package",
      paste("Version:", versions[[i]]),
      "biocViews: Software"
    )

    writeLines(desc, con = "DESCRIPTION")
    writeLines("", con = "NAMESPACE")

    renv_scope_envvars(GIT_AUTHOR_DATE = dates[[i]], GIT_COMMITTER_DATE = dates[[i]])
    renv_system_exec("git", c("add", "-A"), action = "git add")
    renv_system_exec("git", c("commit", "--quiet", "-m", shQuote(versions[[i]])), action = "git commit")
    shas[[i]] <- renv_system_exec("git", c("rev-parse", "--short=7", "HEAD"), action = "git rev-parse")

  }

  # tag the newer commit with the name of the older one; git prefers a tag
  # when resolving such a name, so we shouldn't be fetching tags at all
  renv_system_exec("git", c("tag", shas[[1L]], "HEAD"), action = "git tag")

  # renv installs these alongside any Bioconductor package, so
  # provide stubs for them to keep the repository self-contained
  renv_scope_wd(root)
  stubs <- c(BiocManager = "1.30.26", BiocVersion = "3.24.0")
  for (package in names(stubs)) {
    ensure_directory(package)
    desc <- c(paste("Package:", package), paste("Version:", stubs[[package]]))
    writeLines(desc, con = file.path(package, "DESCRIPTION"))
    writeLines("", con = file.path(package, "NAMESPACE"))
  }

  # publish only the newest version of the package, excluding '.git'
  contrib <- file.path(root, "repos/src/contrib")
  ensure_directory(contrib)

  published <- c(biocpkg = "1.0.1", stubs)
  for (package in names(published)) {
    tarball <- sprintf("%s/%s_%s.tar.gz", contrib, package, published[[package]])
    tar(tarball, file.path(package, c("DESCRIPTION", "NAMESPACE")), compression = "gzip")
  }

  tools::write_PACKAGES(contrib, type = "source")

  record <- list(
    Package              = "biocpkg",
    Version              = "1.0.0",
    Source               = "Bioconductor",
    git_url              = renv_path_normalize(repo),
    git_branch           = "devel",
    git_last_commit      = shas[[1L]],
    git_last_commit_date = "2020-01-01"
  )

  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  repos <- sprintf(fmt, renv_path_normalize(file.path(root, "repos")))

  list(record = record, repos = repos)

}

# use the simulated Bioconductor repository, so that Bioconductor records
# can be restored without BiocManager or access to bioconductor.org
renv_tests_scope_bioconductor_superseded <- function(bioc, scope = parent.frame()) {

  # BiocManager is installed from the regular package repositories
  repos <- c(getOption("repos"), BioCsoft = bioc$repos)

  renv_scope_options(
    repos                     = repos,
    renv.bioconductor.repos   = repos,
    renv.bioconductor.version = "3.24",
    scope                     = scope
  )

}

test_that("Bioconductor packages can be retrieved from their recorded git commit", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  record <- renv_tests_bioconductor_superseded()$record

  path <- renv_scope_tempfile("renv-git-")
  expect_true(renv_retrieve_bioconductor_git_impl(record, path))

  # we should get the recorded version, not the latest one,
  # stamped with the same git provenance as the record
  desc <- renv_description_read(path)
  expect_identical(desc$Version, "1.0.0")

  fields <- grep("^git_", names(record), value = TRUE)
  expect_identical(as.list(desc[fields]), as.list(record[fields]))

})

test_that("superseded Bioconductor package versions are retrieved using git", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  renv_tests_scope()
  bioc <- renv_tests_bioconductor_superseded()
  renv_tests_scope_bioconductor_superseded(bioc)

  # also checks that the installed package retains its git provenance
  renv_test_retrieve(bioc$record)

})

test_that("restore() can restore superseded Bioconductor package versions", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  project <- renv_tests_scope()
  bioc <- renv_tests_bioconductor_superseded()
  renv_tests_scope_bioconductor_superseded(bioc)

  lockfile <- renv_lockfile_init(project = project)
  lockfile$Packages <- list(biocpkg = bioc$record)
  renv_lockfile_write(lockfile, file = "renv.lock")

  # dependencies should be resolved from the recorded commit,
  # rather than guessed (with a warning) from the newer version
  expect_no_warning(restore())

  # we should get the recorded version, rather than the
  # newer version which is all the repository now provides
  expect_true(renv_package_installed("biocpkg"))
  expect_true(renv_package_version("biocpkg") == "1.0.0")

  # a new snapshot should reproduce the lockfile record, so that the
  # package remains restorable from a newly-generated lockfile
  record <- renv_snapshot_description(package = "biocpkg")
  fields <- names(bioc$record)
  expect_identical(as.list(record[fields]), as.list(bioc$record))

})

test_that("Bioconductor git retrieval tolerates a branch that no longer exists", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  # Bioconductor renamed 'master' to 'devel'
  record <- renv_tests_bioconductor_superseded()$record
  record$git_branch <- "master"

  path <- renv_scope_tempfile("renv-git-")
  expect_true(renv_retrieve_bioconductor_git_impl(record, path))

  desc <- renv_description_read(path)
  expect_identical(desc$Version, "1.0.0")

})

test_that("Bioconductor git retrieval tolerates a commit outside the shallow window", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  # this date falls between the two commits, so the shallow fetch succeeds
  # but doesn't provide the recorded commit; the full fetch is then needed
  record <- renv_tests_bioconductor_superseded()$record
  record$git_last_commit_date <- "2020-01-20"

  path <- renv_scope_tempfile("renv-git-")
  expect_true(renv_retrieve_bioconductor_git_impl(record, path))

  desc <- renv_description_read(path)
  expect_identical(desc$Version, "1.0.0")

})

test_that("Bioconductor git retrieval fails if the commit has the wrong version", {

  skip_on_cran()
  skip_if(!nzchar(Sys.which("git")), "git is not installed")

  record <- renv_tests_bioconductor_superseded()$record
  record$Version <- "0.9.0"

  path <- renv_scope_tempfile("renv-git-")
  expect_error(
    renv_retrieve_bioconductor_git_impl(record, path),
    "not the requested version"
  )

  # a failed checkout shouldn't be left behind
  before <- list.files(tempdir(), pattern = "^renv-git-")
  expect_error(
    renv_retrieve_bioconductor_git(record),
    "not the requested version"
  )

  after <- list.files(tempdir(), pattern = "^renv-git-")
  expect_setequal(after, before)

})

test_that("Bioconductor git retrieval rejects urls unsafe for use with git", {

  record <- list(
    Package         = "biocpkg",
    Version         = "1.0.0",
    git_url         = "https://example.com/biocpkg\"; echo oops",
    git_last_commit = "abcdef0"
  )

  path <- renv_scope_tempfile("renv-git-")
  for (url in c(record$git_url, "--upload-pack=oops", "ext::sh -c oops")) {
    record$git_url <- url
    expect_error(renv_retrieve_bioconductor_git_impl(record, path), "invalid git url")
  }

})

test_that("Bioconductor git retrieval only uses shallow fetches with usable metadata", {

  record <- list(
    git_branch           = "devel",
    git_last_commit_date = "2020-01-01"
  )

  # the date is recorded without a time zone, so we allow a day of slack
  full <- "--no-tags origin"
  expect_identical(
    renv_retrieve_bioconductor_git_fetchargs(record),
    list('--no-tags --shallow-since=2019-12-31 origin "devel"', full)
  )

  expect_identical(
    renv_retrieve_bioconductor_git_fetchargs(list(git_branch = "devel")),
    list(full)
  )

  record$git_last_commit_date <- "not a date"
  expect_identical(renv_retrieve_bioconductor_git_fetchargs(record), list(full))

  record$git_last_commit_date <- "2020-01-01"
  record$git_branch <- "devel\"; echo oops"
  expect_identical(renv_retrieve_bioconductor_git_fetchargs(record), list(full))

  # git would read this as an option, not a branch
  record$git_branch <- "--unshallow"
  expect_identical(renv_retrieve_bioconductor_git_fetchargs(record), list(full))

})

test_that("a retrieved package that differs from the record is reported", {

  renv_tests_scope()

  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)

  record <- list(
    Package = "bread",
    Version = "1.0.0",
    Source  = "Repository"
  )

  renv_scope_restore(
    project   = getwd(),
    library   = library,
    records   = list(bread = record),
    packages  = "bread",
    recursive = FALSE
  )

  # the retrieved path holds a different package than the record describes,
  # as would happen if a download cache entry or repository served the wrong file
  path <- renv_tests_path("packages/toast")
  expect_warning(
    renv_retrieve_successful(record, path, install = FALSE),
    "is 'toast', but 'bread' was requested"
  )

  # a matching package is accepted quietly
  record$Package <- "toast"
  expect_no_warning(renv_retrieve_successful(record, path, install = FALSE))

})

test_that("downloaded archives are named after the repository's File field", {
  skip_on_cran()
  project <- renv_tests_scope()

  # a repository advertising archives whose names the type-to-extension
  # mapping for this build of R would not guess, as custom binary types
  # permit; R saves such files under their advertised names (#2423)
  pkgdir <- renv_scope_tempfile("renv-file-pkg-")
  ensure_directory(file.path(pkgdir, "xzpkg"))
  writeLines(
    c(
      "Package: xzpkg",
      "Version: 1.0.0",
      "Title: A Package",
      "Description: Test.",
      "License: MIT"
    ),
    file.path(pkgdir, "xzpkg", "DESCRIPTION")
  )

  repopath <- renv_scope_tempfile("renv-file-repo-")
  contrib <- file.path(repopath, "src/contrib")
  ensure_directory(contrib)

  renv_scope_wd(pkgdir)
  tar(
    tarfile     = file.path(contrib, "xzpkg_1.0.0.tar.xz"),
    files       = "xzpkg",
    compression = "xz"
  )

  writeLines(
    c("Package: xzpkg", "Version: 1.0.0", "File: xzpkg_1.0.0.tar.xz", ""),
    file.path(contrib, "PACKAGES")
  )

  renv_scope_options(repos = c(local = paste0("file://", repopath)))
  record <- list(Package = "xzpkg", Version = "1.0.0")

  # the parallel download planner names the local copy after the file ...
  info <- renv_graph_url_repository(record)
  expect_equal(basename(info$url), "xzpkg_1.0.0.tar.xz")
  expect_equal(basename(info$destfile), "xzpkg_1.0.0.tar.xz")

  # ... as does sequential retrieval
  destdir <- renv_scope_tempfile("renv-file-destdir-")
  ensure_directory(destdir)
  result <- retrieve("xzpkg", destdir = destdir, project = project)
  expect_equal(basename(result[["xzpkg"]]), "xzpkg_1.0.0.tar.xz")

  # binaries served under a custom extension are named the same way
  skip_if(identical(.Platform$pkgType, "source"))

  bincontrib <- paste0(repopath, contrib.url("", type = "binary"))
  ensure_directory(bincontrib)

  # 'Meta' marks an installed package; only the archive name matters here
  ensure_directory(file.path(pkgdir, "xzpkg", "Meta"))
  file.create(file.path(pkgdir, "xzpkg", "Meta", "package.rds"))
  tar(
    tarfile     = file.path(bincontrib, "xzpkg_1.0.0.tar.xz"),
    files       = "xzpkg",
    compression = "xz"
  )

  writeLines(
    c("Package: xzpkg", "Version: 1.0.0", "File: xzpkg_1.0.0.tar.xz", ""),
    file.path(bincontrib, "PACKAGES")
  )

  renv_scope_options(pkgType = "binary")
  info <- renv_graph_url_repository(record)
  expect_equal(info$pkgtype, "binary")
  expect_equal(basename(info$url), "xzpkg_1.0.0.tar.xz")
  expect_equal(basename(info$destfile), "xzpkg_1.0.0.tar.xz")

  destdir <- renv_scope_tempfile("renv-file-destdir-")
  ensure_directory(destdir)
  result <- retrieve("xzpkg", destdir = destdir, project = project)
  expect_equal(basename(result[["xzpkg"]]), "xzpkg_1.0.0.tar.xz")
  expect_equal(renv_package_type(result[["xzpkg"]]), "binary")

})

test_that("local archives are named after the record, with the advertised extension", {

  renv_scope_binding(the, "destdir", "downloads")
  record <- list(Package = "pkg", Version = "1.0")

  # the advertised extension is kept even when it is not the one assumed
  # for packages of that type on this build of R (#2423)
  path <- renv_retrieve_path(record, name = "pkg_1.0.tar.xz")
  expect_equal(path, "downloads/pkg_1.0.tar.xz")

  path <- renv_retrieve_path(record, type = "binary", name = "pkg_1.0.zip")
  expect_equal(path, "downloads/pkg_1.0.zip")

  # only the extension is taken, so an index entry cannot direct the
  # download outside its directory, and query strings never reach the name
  path <- renv_retrieve_path(record, name = "../pkg_1.0.tar.gz")
  expect_equal(path, "downloads/pkg_1.0.tar.gz")

  path <- renv_retrieve_path(record, name = "..\\pkg_1.0.tgz?sha256=abc")
  expect_equal(path, "downloads/pkg_1.0.tgz")

  # names without a recognized archive extension get the default one
  for (name in c("", ".", "..", "?sha256=abc", "subdir/", "pkg_1.0.rds")) {
    path <- renv_retrieve_path(record, name = name)
    expect_equal(path, "downloads/pkg_1.0.tar.gz", info = name)
  }

  # records built from a commit (r-universe) keep it in the name, so that
  # builds of one version from different commits do not share a file
  sha <- strrep("a", 40L)
  record <- list(Package = "pkg", Version = "1.0", RemoteSha = sha)
  path <- renv_retrieve_path(record, type = "binary", name = "pkg_1.0.tgz")
  expect_equal(path, sprintf("downloads/pkg_%s.tgz", sha))

})

test_that("binary archives keep their advertised extension on every platform", {

  renv_tests_scope()

  # the failure in #2423 was a '.zip' binary stored as '.tgz'. the round-trip
  # test above can only exercise binaries on platforms with a binary type, so
  # drive the planner and the sequential retrieval directly with a tagged
  # index entry here, independent of .Platform$pkgType
  repo <- "https://example.com/bin/contrib"
  desc <- list(Package = "zippkg", Version = "1.0", Source = "Repository")
  record <- renv_record_tag(
    record = c(desc, File = "zippkg_1.0.zip"),
    type   = "binary",
    url    = repo,
    name   = "example"
  )

  info <- renv_graph_url_repository_record(desc, record)
  expect_equal(info$url, file.path(repo, "zippkg_1.0.zip"))
  expect_equal(info$pkgtype, "binary")
  expect_equal(basename(info$destfile), "zippkg_1.0.zip")
  expect_equal(dirname(info$destfile), renv_paths_binary("repository", "zippkg"))

  captured <- NULL
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = "renv_retrieve_package",
    replacement = function(record, url, path, defer = FALSE) {
      captured <<- list(url = url, path = path)
      TRUE
    }
  )

  expect_true(renv_retrieve_repos_impl(record))
  expect_equal(captured$url, info$url)
  expect_equal(captured$path, info$destfile)

})

test_that("Path entries shape the download URL but not the file name", {
  skip_on_cran()
  project <- renv_tests_scope()

  pkgdir <- renv_scope_tempfile("renv-path-pkg-")
  ensure_directory(file.path(pkgdir, "pathpkg"))
  writeLines(
    c(
      "Package: pathpkg",
      "Version: 1.0.0",
      "Title: A Package",
      "Description: Test.",
      "License: MIT"
    ),
    file.path(pkgdir, "pathpkg", "DESCRIPTION")
  )

  renv_scope_wd(pkgdir)
  tarball <- file.path(pkgdir, "pathpkg_1.0.0.tar.gz")
  tar(tarfile = tarball, files = "pathpkg", compression = "gzip")

  record <- list(Package = "pathpkg", Version = "1.0.0")

  # a relative Path names a sub-directory of the index, as pak's repository does
  repopath <- renv_scope_tempfile("renv-path-repo-")
  contrib <- file.path(repopath, "src/contrib")
  ensure_directory(file.path(contrib, "extras"))
  file.copy(tarball, file.path(contrib, "extras"))
  writeLines(
    c(
      "Package: pathpkg",
      "Version: 1.0.0",
      "Path: extras",
      "File: pathpkg_1.0.0.tar.gz",
      ""
    ),
    file.path(contrib, "PACKAGES")
  )

  renv_scope_options(repos = c(local = paste0("file://", repopath)))
  info <- renv_graph_url_repository(record)
  expect_true(endsWith(info$url, "/src/contrib/extras/pathpkg_1.0.0.tar.gz"))
  expect_equal(basename(info$destfile), "pathpkg_1.0.0.tar.gz")

  destdir <- renv_scope_tempfile("renv-path-destdir-")
  ensure_directory(destdir)
  result <- retrieve("pathpkg", destdir = destdir, project = project)
  expect_equal(basename(result[["pathpkg"]]), "pathpkg_1.0.0.tar.gz")
  expect_true(file.exists(result[["pathpkg"]]))

  # r-universe's Path entries repeat the file name and append a query string;
  # the URL must carry it (here, as a directory of that name), the file name
  # must not. such a directory cannot exist on Windows
  skip_on_windows()

  query <- "pathpkg_1.0.0.tar.gz?sha256=abc&file="
  repopath <- renv_scope_tempfile("renv-path-repo-")
  contrib <- file.path(repopath, "src/contrib")
  ensure_directory(file.path(contrib, query))
  file.copy(tarball, file.path(contrib, query))
  writeLines(
    c(
      "Package: pathpkg",
      "Version: 1.0.0",
      paste("Path:", query),
      "File: pathpkg_1.0.0.tar.gz",
      ""
    ),
    file.path(contrib, "PACKAGES")
  )

  renv_scope_options(repos = c(local = paste0("file://", repopath)))
  info <- renv_graph_url_repository(record)
  expect_true(endsWith(info$url, paste0("/src/contrib/", query, "/pathpkg_1.0.0.tar.gz")))
  expect_equal(basename(info$destfile), "pathpkg_1.0.0.tar.gz")

  destdir <- renv_scope_tempfile("renv-path-destdir-")
  ensure_directory(destdir)
  result <- retrieve("pathpkg", destdir = destdir, project = project)
  expect_equal(basename(result[["pathpkg"]]), "pathpkg_1.0.0.tar.gz")
  expect_true(file.exists(result[["pathpkg"]]))

})
