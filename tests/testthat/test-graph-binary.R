
test_that("binary installs overlap and respect the worker limit", {

  skip_on_cran()
  skip_if(getRversion() < "4.0")
  skip_if(identical(.Platform$pkgType, "source"))
  renv_tests_scope(isolated = TRUE)
  renv_scope_binding(the, "install_pkg_type", "binary")
  renv_scope_options(
    pkgType = .Platform$pkgType,
    renv.config.cache.enabled = TRUE,
    renv.config.install.jobs = 2L
  )

  # The first two workers must both start before either may extract its
  # archive. This fails if binary work is performed in the parent process,
  # without relying on elapsed-time comparisons.
  markers <- renv_scope_tempfile("renv-workers-")
  ensure_directory(markers)
  worker <- renv_graph_install_binary_worker
  replacement <- function(prepared, copy, tar) {}
  body(replacement) <- expr({
    stopifnot(identical(prepared$method, "extract"), !dir.exists(prepared$path))
    stopifnot(!"renv" %in% loadedNamespaces())
    markers <- !!markers
    writeLines(as.character(Sys.getpid()), file.path(markers, prepared$package))
    deadline <- Sys.time() + 30
    while (!all(file.exists(file.path(markers, c("bread", "egg"))))) {
      if (Sys.time() > deadline)
        stop("binary workers did not overlap")
      Sys.sleep(0.05)
    }
    worker <- !!worker
    worker(prepared, copy, tar)
  })
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_binary_worker", replacement)

  # Count requested but not yet finalized packages in the parent. With three
  # ready binaries and two jobs, the third must wait for a slot, and the pool
  # starts no more than two workers.
  active <- maximum <- 0L
  request <- renv_graph_install_binary_request
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_binary_request", function(...) {
    active <<- active + 1L
    maximum <<- max(maximum, active)
    request(...)
  })
  finalize <- renv_graph_install_finalize
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_finalize", function(...) {
    active <<- active - 1L
    finalize(...)
  })
  workers <- character()
  launch <- renv_graph_install_pool_launch
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_pool_launch", function(id, ...) {
    workers <<- c(workers, id)
    launch(id, ...)
  })

  packages <- c("bread", "egg", "oatmeal")
  records <- renv_graph_install(renv_graph_init(packages))
  expect_setequal(names(records), packages)
  expect_equal(maximum, 2L)
  expect_equal(active, 0L)
  expect_length(workers, 2L)
  for (package in packages) {
    pid <- readLines(file.path(markers, package))
    expect_false(identical(pid, as.character(Sys.getpid())))
    expect_true(renv_package_installed(package), info = package)
    cache <- renv_cache_path(file.path(renv_libpaths_active(), package))
    expect_true(renv_cache_package_validate(cache), info = package)
  }

})

test_that("binary and source installs each respect the job limit", {

  skip_on_cran()
  skip_if(getRversion() < "4.0")
  skip_if(identical(.Platform$pkgType, "source"))

  for (jobs in c(1L, 0L, 2L)) local({
    renv_tests_scope(isolated = TRUE)
    source <- renv_graph_urls(renv_graph_init("egg"))$egg
    renv_scope_binding(the, "install_pkg_type", "binary")
    renv_scope_options(
      pkgType = .Platform$pkgType,
      renv.config.cache.enabled = FALSE,
      renv.config.install.jobs = jobs
    )

    # Pin egg's download to the source archive; the binary repository
    # also provides egg, so its description alone doesn't force a build.
    urls <- renv_graph_urls
    renv_scope_binding(asNamespace("renv"), "renv_graph_urls", function(descriptions) {
      info <- urls(descriptions)
      info$egg <- source
      info
    })

    # Count packages of each type between dispatch (to a build worker, or to
    # a binary worker) and finalization.
    active <- maximum <- c(binary = 0L, source = 0L)
    launch <- renv_graph_install_launch_socket
    renv_scope_binding(asNamespace("renv"), "renv_graph_install_launch_socket", function(prepared, ...) {
      active[["source"]] <<- active[["source"]] + 1L
      maximum <<- pmax(maximum, active)
      launch(prepared, ...)
    })
    request <- renv_graph_install_binary_request
    renv_scope_binding(asNamespace("renv"), "renv_graph_install_binary_request", function(prepared) {
      active[["binary"]] <<- active[["binary"]] + 1L
      maximum <<- pmax(maximum, active)
      request(prepared)
    })
    finalize <- renv_graph_install_finalize
    renv_scope_binding(asNamespace("renv"), "renv_graph_install_finalize", function(record, prepared, ...) {
      active[[prepared$type]] <<- active[[prepared$type]] - 1L
      finalize(record, prepared, ...)
    })

    # Two binaries and one source package: each pool fills to its limit.
    packages <- c("bread", "egg", "oatmeal")
    descriptions <- renv_graph_init(packages)
    records <- renv_graph_install(descriptions)
    expect_setequal(names(records), packages)
    expect_equal(maximum, c(binary = max(jobs, 1L), source = 1L))
    expect_equal(active, c(binary = 0L, source = 0L))
    for (package in packages)
      expect_true(renv_package_installed(package), info = package)
  })

})

test_that("binary installs are not held up behind source builds", {

  skip_on_cran()
  skip_if(getRversion() < "4.0")
  skip_if(identical(.Platform$pkgType, "source"))
  renv_tests_scope(isolated = TRUE)
  source <- renv_graph_urls(renv_graph_init("egg"))$egg
  renv_scope_binding(the, "install_pkg_type", "binary")
  renv_scope_options(
    pkgType = .Platform$pkgType,
    renv.config.cache.enabled = FALSE,
    renv.config.install.jobs = 1L
  )

  urls <- renv_graph_urls
  renv_scope_binding(asNamespace("renv"), "renv_graph_urls", function(descriptions) {
    info <- urls(descriptions)
    info$egg <- source
    info
  })

  # Record the order of events. With a single job, oatmeal is still installed
  # while egg builds, as the two use separate pools.
  events <- character()
  launch <- renv_graph_install_launch_socket
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_launch_socket", function(prepared, ...) {
    events <<- c(events, paste("launch", prepared$package))
    launch(prepared, ...)
  })
  request <- renv_graph_install_binary_request
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_binary_request", function(prepared) {
    events <<- c(events, paste("request", prepared$package))
    request(prepared)
  })
  finalize <- renv_graph_install_finalize
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_finalize", function(record, ...) {
    events <<- c(events, paste("finalize", record$Package))
    finalize(record, ...)
  })

  packages <- c("bread", "egg", "oatmeal")
  records <- renv_graph_install(renv_graph_init(packages))
  expect_setequal(names(records), packages)
  expect_lt(match("launch egg", events), match("request oatmeal", events))
  expect_lt(match("request oatmeal", events), match("finalize egg", events))

})

test_that("failed binary extraction blocks dependents and rolls back staged installs", {

  skip_on_cran()
  skip_if(getRversion() < "4.0")
  skip_if(identical(.Platform$pkgType, "source"))
  renv_tests_scope(isolated = TRUE)
  renv_scope_binding(the, "install_pkg_type", "binary")
  renv_scope_options(
    pkgType = .Platform$pkgType,
    renv.config.cache.enabled = FALSE,
    renv.config.install.jobs = 2L,
    renv.config.install.staged = TRUE,
    renv.config.install.transactional = TRUE
  )

  # Keep an existing installation to verify rollback preserves it.
  library <- renv_libpaths_active()
  old <- file.path(library, "bread")
  ensure_directory(old)
  write.dcf(data.frame(Package = "bread", Version = "0.0.1"), file.path(old, "DESCRIPTION"))
  writeLines("old installation", file.path(old, "sentinel"))

  requested <- character()
  request <- renv_graph_install_binary_request
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_binary_request", function(prepared) {
    requested <<- c(requested, prepared$package)
    # Corrupt the downloaded archive after classification, so the failure
    # occurs during extraction in the worker.
    if (identical(prepared$package, "bread"))
      writeLines("invalid binary archive", prepared$path)
    request(prepared)
  })
  workers <- list()
  launch <- renv_graph_install_pool_launch
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_pool_launch", function(...) {
    worker <- launch(...)
    workers[[worker$id]] <<- worker
    worker
  })

  records <- renv_graph_install(renv_graph_init(c("toast", "egg")))
  expect_length(records, 0L)
  expect_setequal(attr(records, "failed"), c("bread", "toast"))
  expect_setequal(attr(records, "rolledback"), "egg")
  expect_setequal(requested, c("bread", "egg"))
  expect_equal(readLines(file.path(old, "sentinel")), "old installation")
  expect_false(file.exists(file.path(library, "egg")))
  expect_false(file.exists(file.path(library, "toast")))
  for (worker in workers)
    expect_false(file.exists(worker$script))

})

test_that("finalization errors wait for outstanding workers and restore backups", {

  skip_on_cran()
  skip_if(getRversion() < "4.0")
  skip_if(identical(.Platform$pkgType, "source"))
  renv_tests_scope(isolated = TRUE)
  renv_scope_binding(the, "install_pkg_type", "binary")
  renv_scope_options(
    pkgType = .Platform$pkgType,
    renv.config.cache.enabled = FALSE,
    renv.config.install.jobs = 2L,
    renv.config.install.staged = FALSE
  )

  # Without staging, existing installations are moved aside in the library
  # while their replacements are installed.
  library <- renv_libpaths_active()
  for (package in c("bread", "egg")) {
    old <- file.path(library, package)
    ensure_directory(old)
    write.dcf(data.frame(Package = package, Version = "0.0.1"), file.path(old, "DESCRIPTION"))
    writeLines("old installation", file.path(old, "sentinel"))
  }

  # Hold egg's worker until bread's finalization has failed, so that egg is
  # still in flight when the error occurs.
  marker <- renv_scope_tempfile("renv-finalize-")
  worker <- renv_graph_install_binary_worker
  replacement <- function(prepared, copy, tar) {}
  body(replacement) <- expr({
    if (identical(prepared$package, "egg")) {
      deadline <- Sys.time() + 30
      while (!file.exists(!!marker)) {
        if (Sys.time() > deadline)
          stop("bread was not finalized")
        Sys.sleep(0.05)
      }
    }
    worker <- !!worker
    worker(prepared, copy, tar)
  })
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_binary_worker", replacement)

  workers <- list()
  launch <- renv_graph_install_pool_launch
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_pool_launch", function(...) {
    worker <- launch(...)
    workers[[worker$id]] <<- worker
    worker
  })

  # Fail bread's finalization, leaving its replacement in the library as a
  # failure to update its DESCRIPTION would.
  finalized <- character()
  finalize <- renv_graph_install_finalize
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_finalize", function(record, prepared, library, ...) {
    finalized <<- c(finalized, record$Package)
    if (identical(record$Package, "bread")) {
      writeLines("", marker)
      stop("finalization failed")
    }
    finalize(record, prepared, library, ...)
  })

  descriptions <- renv_graph_init(c("bread", "egg"))
  connections <- getAllConnections()
  expect_error(renv_graph_install(descriptions), "finalization failed")
  expect_setequal(getAllConnections(), connections)
  expect_equal(finalized, c("bread", "egg"))
  expect_length(workers, 2L)
  for (worker in workers)
    expect_false(file.exists(worker$script))

  # bread's previous installation is restored; egg's replacement completes
  expect_equal(readLines(file.path(library, "bread", "sentinel")), "old installation")
  expect_false(file.exists(file.path(library, "egg", "sentinel")))
  expect_true(renv_package_installed("egg"))
  backups <- list.files(library, pattern = "^[.]renv-backup-", all.files = TRUE)
  expect_length(backups, 0L)

})

test_that("installs that fail while workers drain are still reported", {

  skip_on_cran()
  skip_if(getRversion() < "4.0")
  skip_if(identical(.Platform$pkgType, "source"))
  renv_tests_scope(isolated = TRUE)
  renv_scope_binding(the, "install_pkg_type", "binary")
  renv_scope_options(
    pkgType = .Platform$pkgType,
    renv.verbose = TRUE,
    renv.config.cache.enabled = FALSE,
    renv.config.install.jobs = 2L
  )

  # Hold egg's worker until bread's finalization has failed, then fail it too.
  marker <- renv_scope_tempfile("renv-finalize-")
  worker <- renv_graph_install_binary_worker
  replacement <- function(prepared, copy, tar) {}
  body(replacement) <- expr({
    if (identical(prepared$package, "egg")) {
      deadline <- Sys.time() + 30
      while (!file.exists(!!marker)) {
        if (Sys.time() > deadline)
          stop("bread was not finalized")
        Sys.sleep(0.05)
      }
      stop("egg extraction failed")
    }
    worker <- !!worker
    worker(prepared, copy, tar)
  })
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_binary_worker", replacement)

  renv_scope_binding(asNamespace("renv"), "renv_graph_install_finalize", function(record, ...) {
    writeLines("", marker)
    stop("finalization failed")
  })

  # The finalization error is re-signaled, but egg's failure is reported first.
  descriptions <- renv_graph_init(c("bread", "egg"))
  expect_output(
    expect_error(renv_graph_install(descriptions), "finalization failed"),
    "egg extraction failed"
  )

})

test_that("binary workers extract nested tar archives and copy directories", {

  renv_tests_scope()
  root <- renv_scope_tempfile("renv-binary-fixture-")
  package <- file.path(root, "bundle", "nested")
  ensure_directory(package)
  write.dcf(data.frame(Package = "bread", Version = "1.0.0"), file.path(package, "DESCRIPTION"))
  writeLines("payload", file.path(package, ".hidden"))
  renv_scope_wd(root)

  archive <- file.path(root, "binary package.tar.gz")
  utils::tar(archive, "bundle", compression = "gzip", tar = "internal")

  # Exercise the same function without access to the renv namespace.
  worker <- renv_graph_install_binary_worker
  environment(worker) <- baseenv()

  for (method in c("extract", "r", "auto")) local({
    library <- renv_scope_tempfile("renv-binary-library-")
    ensure_directory(library)
    prepared <- list(
      package = "bread", library = library,
      method = if (method == "extract") "extract" else "copy",
      archive = "tar", subdir = "nested",
      path = if (method == "extract") archive else file.path(root, "bundle")
    )
    worker(prepared, copy = if (method == "extract") "r" else method, tar = "internal")
    expect_equal(readLines(file.path(library, "bread", ".hidden")), "payload")
  })

  # An archive with DESCRIPTION at its root has no package directory to install.
  flat <- file.path(root, "flat")
  ensure_directory(flat)
  write.dcf(data.frame(Package = "bread", Version = "1.0.0"), file.path(flat, "DESCRIPTION"))
  renv_scope_wd(flat)
  archive <- file.path(root, "flat.tar.gz")
  utils::tar(archive, "DESCRIPTION", compression = "gzip", tar = "internal")

  library <- renv_scope_tempfile("renv-binary-library-")
  ensure_directory(library)
  prepared <- list(
    package = "bread", library = library,
    method = "extract", archive = "tar", subdir = "", path = archive
  )
  expect_error(worker(prepared, copy = "r", tar = "internal"), "couldn't find DESCRIPTION")

})

test_that("worker cleanup logs close failures and still removes files", {

  log <- renv_scope_tempfile(fileext = ".log")
  renv_scope_binding(the, "log_level", 1L)
  renv_scope_binding(the, "log_scopes", "graph")
  renv_scope_binding(the, "log_file", log)

  script <- renv_scope_tempfile(fileext = ".R")
  writeLines("invisible()", script)

  conn <- rawConnection(raw(), open = "r")
  close(conn)
  error <- catch(close(conn))
  worker <- list(package = "bread", conn = conn, script = script)

  expect_true(renv_graph_install_worker_cleanup(worker))
  expect_false(file.exists(script))
  output <- readLines(log)
  expect_match(output, "graph: failed to close worker connection for 'bread':", fixed = TRUE)
  expect_match(output, conditionMessage(error), fixed = TRUE)

  # A worker that never connected has no connection to close.
  worker$conn <- NULL
  expect_true(renv_graph_install_worker_cleanup(worker))
  expect_identical(readLines(log), output)

})

test_that("worker directory removal retries locked files on Windows", {

  skip_if_not(renv_platform_windows())

  workdir <- renv_scope_tempfile("renv-install-workers-")
  ensure_directory(workdir)
  script <- file.path(workdir, "renv-install-1.R")
  writeLines("invisible()", script)

  # Keep the script open just as R does when launched with -f. Release the
  # lock during the retry wait, avoiding a timing-dependent subprocess.
  lock <- file(script, open = "r")
  defer(try(close(lock), silent = TRUE))

  remove <- renv_graph_install_workdir_remove
  scope <- new.env(parent = environment(remove))
  environment(remove) <- scope
  retried <- FALSE
  scope$Sys.sleep <- function(...) {
    retried <<- TRUE
    close(lock)
  }

  expect_true(remove(workdir))
  expect_true(retried)
  expect_false(file.exists(workdir))

})

test_that("binary workers honor custom copy methods and propagate copy failures", {

  renv_tests_scope()
  source <- renv_scope_tempfile("renv-binary-source-")
  library <- renv_scope_tempfile("renv-binary-library-")
  ensure_directory(c(source, library))
  prepared <- list(package = "bread", library = library, path = source, method = "copy", subdir = "")

  copied <- FALSE
  copy <- function(source, target) {
    copied <<- TRUE
    dir.create(target)
    writeLines("custom copy", file.path(target, "sentinel"))
  }
  renv_graph_install_binary_worker(prepared, copy, "internal")
  expect_true(copied)
  expect_equal(readLines(file.path(library, "bread", "sentinel")), "custom copy")

  expect_error(
    renv_graph_install_binary_worker(prepared, function(...) warning("copy failed"), "internal"),
    "copy failed"
  )

  # a copy method that produces no package directory fails without retrying
  expect_error(
    renv_graph_install_binary_worker(prepared, function(...) NULL, "internal"),
    "no package directory"
  )

})

test_that("binary workers preserve custom copy method closures and are reused", {

  skip_on_cran()
  skip_if(getRversion() < "4.0")
  renv_tests_scope()
  source <- renv_scope_tempfile("renv-binary-source-")
  ensure_directory(source)

  copy <- local({
    message <- "captured by the copy method"
    function(source, target) {
      dir.create(target)
      writeLines(message, file.path(target, "sentinel"))
    }
  }, envir = new.env(parent = baseenv()))
  renv_scope_options(renv.config.copy.method = copy)

  workdir <- renv_scope_tempfile("renv-workers-")
  ensure_directory(workdir)
  server <- renv_socket_server()
  defer(close(server$socket))
  worker <- renv_graph_install_pool_launch("1", server$port, workdir, 60)
  defer(renv_graph_install_pool_close(worker))
  worker$conn <- renv_socket_accept(server$socket, open = "r+b", timeout = 30)
  expect_identical(unserialize(worker$conn), list(worker = "1"))

  # the same worker serves successive requests; the fixture has nothing to
  # load, so don't load-test it yet
  renv_scope_envvars(RENV_INSTALL_TEST_LOAD = "FALSE")
  libraries <- c(renv_libpaths_active(), renv_scope_tempfile("renv-binary-library-"))
  ensure_directory(libraries)
  for (library in libraries) {
    prepared <- renv_graph_install_prepare(list(Package = "bread"), source, library, "binary", FALSE)
    worker$request <- renv_graph_install_binary_request(prepared)
    worker <- renv_graph_install_pool_send(worker)
    expect_identical(unserialize(worker$conn), character())
    expect_equal(readLines(file.path(library, "bread", "sentinel")), "captured by the copy method")
  }

  # the worker load-tests what it installs: this 'bread' has nothing to load,
  # and that is reported separately from a failed install
  renv_scope_envvars(RENV_INSTALL_TEST_LOAD = "TRUE")
  library <- renv_scope_tempfile("renv-binary-library-")
  ensure_directory(library)
  prepared <- renv_graph_install_prepare(list(Package = "bread"), source, library, "binary", FALSE)
  worker$request <- renv_graph_install_binary_request(prepared)
  worker <- renv_graph_install_pool_send(worker)
  result <- unserialize(worker$conn)
  expect_s3_class(result, "renv_install_test_error")
  expect_match(conditionMessage(result), "testing if 'bread' can be loaded")
  expect_true(file.exists(file.path(library, "bread", "sentinel")))

})
