
test_that("an existing lockfile can be updated", {

  renv_tests_scope("bread")
  init()

  # change bread to 0.1.0
  record(list(bread = "bread@0.1.0"))

  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_lockfile_records(lockfile)
  expect_length(records, 1L)
  expect_equal(records$bread$Version, "0.1.0")

  # add a new record
  record(list(toast = "toast@1.0.0"))

  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_lockfile_records(lockfile)
  expect_length(records, 2L)
  expect_equal(records$bread$Version, "0.1.0")
  expect_equal(records$toast$Version, "1.0.0")

  # use short-hand
  lockfile <- record("toast@1.0.1", lockfile = lockfile)
  records <- renv_lockfile_records(lockfile)
  expect_equal(records$toast$Version, "1.0.1")

  lockfile <- record(list(toast = "1.0.2"), lockfile = lockfile)
  records <- renv_lockfile_records(lockfile)
  expect_equal(records$toast$Version, "1.0.2")

  # remove a record
  lockfile <- record(list(toast = NULL), lockfile = lockfile)
  records <- renv_lockfile_records(lockfile)
  expect_true(is.null(records$toast))

})

test_that("record(<package>) also records version", {

  renv_tests_scope()

  # create empty lockfile
  snapshot()

  # add a record
  record("breakfast")

  # check that the version of breakfast was recorded
  lockfile <- renv_lockfile_load(project = getwd())
  breakfast <- lockfile$Packages$breakfast
  expect_identical(breakfast$Version, "1.0.0")

})

test_that("record() enriches resolved records with DESCRIPTION fields", {

  renv_tests_scope("breakfast")
  init()

  # snapshot record (built from the installed DESCRIPTION) is the
  # baseline for what an "enriched" record should look like
  lockfile <- renv_lockfile_read("renv.lock")
  snap_record <- renv_lockfile_records(lockfile)$breakfast

  record("breakfast")

  lockfile <- renv_lockfile_read("renv.lock")
  rec_record <- renv_lockfile_records(lockfile)$breakfast

  # record() must produce more than the minimal four fields
  minimal <- c("Package", "Version", "Source", "Repository")
  expect_true(length(setdiff(names(rec_record), minimal)) > 0L)

  # the standard PACKAGES-derived dep / license fields should be present
  # and agree with what snapshot() wrote
  shared <- c("Package", "Version", "Source", "Repository",
              "Depends", "Suggests", "License")
  shared <- intersect(shared, names(snap_record))
  for (field in shared)
    expect_identical(rec_record[[field]], snap_record[[field]],
                     info = sprintf("field '%s' differs", field))

})

test_that("record(enrich = FALSE) keeps minimal records", {

  renv_tests_scope("breakfast")
  init()

  record("breakfast", enrich = FALSE)

  lockfile <- renv_lockfile_read("renv.lock")
  rec_record <- renv_lockfile_records(lockfile)$breakfast

  expect_setequal(
    names(rec_record),
    c("Package", "Version", "Source", "Repository")
  )

})

test_that("enrich cache key varies with options('repos')", {

  # the description fetcher reads from options('repos'), so the memoize
  # key must too. version-pinned repository specs have no Repository
  # field, so without this guard a record('pkg@1.0.0') call would return
  # cached data fetched against a different repos config.
  cranlike <- list(
    Package = "foo", Version = "1.0.0", Source = "Repository"
  )

  renv_scope_options(repos = c(CRAN = "https://cran.example/a"))
  before <- renv_record_enrich_key(cranlike)

  renv_scope_options(repos = c(CRAN = "https://cran.example/b"))
  after <- renv_record_enrich_key(cranlike)

  expect_false(identical(before, after))

  # github (and other non-repository sources) carry full source identity
  # on the record itself; their key should not depend on options('repos')
  github <- list(
    Package = "foo", Version = "1.0.0", Source = "GitHub",
    RemoteUsername = "user", RemoteRepo = "repo", RemoteSha = "abc"
  )

  renv_scope_options(repos = c(CRAN = "https://cran.example/a"))
  before <- renv_record_enrich_key(github)

  renv_scope_options(repos = c(CRAN = "https://cran.example/b"))
  after <- renv_record_enrich_key(github)

  expect_identical(before, after)

})

test_that("record() does not enrich caller-supplied list records", {

  # a fully-resolved list record should pass through untouched: no
  # network access, no field rewrite. this matches the documented "record
  # this entry as provided" behavior and lets users seed unusual entries
  # (custom fields, sources renv doesn't understand) without round-tripping
  # through the snapshot transform.
  renv_tests_scope()
  snapshot()

  custom <- list(
    Package    = "fictional",
    Version    = "9.9.9",
    Source     = "Repository",
    Repository = "CRAN",
    Custom     = "preserved"
  )
  record(list(fictional = custom))

  lockfile <- renv_lockfile_read("renv.lock")
  fictional <- renv_lockfile_records(lockfile)$fictional

  expect_identical(fictional$Version, "9.9.9")
  expect_identical(fictional$Custom, "preserved")

})

test_that("record() stores Repository as the named form, not the URL", {

  # version-pinned specs skip the 'latest' branch in
  # renv_remotes_resolve_repository(), so the resolved record has no
  # Repository field. enrichment must still produce the named form (e.g.
  # 'CRAN') that snapshot() writes -- otherwise status() will report a
  # spurious repo mismatch against an installed library.
  renv_tests_scope("egg")
  init()
  record("egg@2.0.0")

  lockfile <- renv_lockfile_read("renv.lock")
  egg <- renv_lockfile_records(lockfile)$egg

  expect_identical(egg$Repository, "CRAN")
  expect_null(egg$Name)

})
