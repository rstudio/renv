
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

  # Count launched but not yet finalized packages in the parent. With
  # three ready binaries and two jobs, the third must wait for a slot.
  active <- maximum <- 0L
  launch <- renv_graph_install_launch_socket
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_launch_socket", function(...) {
    active <<- active + 1L
    maximum <<- max(maximum, active)
    launch(...)
  })
  finalize <- renv_graph_install_finalize
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_finalize", function(...) {
    active <<- active - 1L
    finalize(...)
  })

  packages <- c("bread", "egg", "oatmeal")
  records <- renv_graph_install(renv_graph_init(packages))
  expect_setequal(names(records), packages)
  expect_equal(maximum, 2L)
  expect_equal(active, 0L)
  for (package in packages) {
    pid <- readLines(file.path(markers, package))
    expect_false(identical(pid, as.character(Sys.getpid())))
    expect_true(renv_package_installed(package), info = package)
    cache <- renv_cache_path(file.path(renv_libpaths_active(), package))
    expect_true(renv_cache_package_validate(cache), info = package)
  }

})

test_that("binary and source installs share the job limit", {

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

    active <- maximum <- 0L
    types <- character()
    launch <- renv_graph_install_launch_socket
    renv_scope_binding(asNamespace("renv"), "renv_graph_install_launch_socket", function(prepared, ...) {
      active <<- active + 1L
      maximum <<- max(maximum, active)
      types <<- c(types, prepared$type)
      launch(prepared, ...)
    })
    finalize <- renv_graph_install_finalize
    renv_scope_binding(asNamespace("renv"), "renv_graph_install_finalize", function(...) {
      active <<- active - 1L
      finalize(...)
    })

    packages <- c("bread", "egg", "oatmeal")
    descriptions <- renv_graph_init(packages)
    records <- renv_graph_install(descriptions)
    expect_setequal(names(records), packages)
    expect_setequal(types, c("binary", "source"))
    expect_equal(maximum, max(jobs, 1L))
    expect_equal(active, 0L)
  })

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

  launched <- character()
  workers <- list()
  launch <- renv_graph_install_launch_socket
  renv_scope_binding(asNamespace("renv"), "renv_graph_install_launch_socket", function(prepared, port) {
    launched <<- c(launched, prepared$package)
    # Corrupt the downloaded archive after classification, so the failure
    # occurs during extraction in the worker.
    if (identical(prepared$package, "bread"))
      writeLines("invalid binary archive", prepared$path)
    worker <- launch(prepared, port)
    workers[[prepared$package]] <<- worker
    worker
  })

  records <- renv_graph_install(renv_graph_init(c("toast", "egg")))
  expect_length(records, 0L)
  expect_setequal(attr(records, "failed"), c("bread", "toast"))
  expect_setequal(attr(records, "rolledback"), "egg")
  expect_setequal(launched, c("bread", "egg"))
  expect_equal(readLines(file.path(old, "sentinel")), "old installation")
  expect_false(file.exists(file.path(library, "egg")))
  expect_false(file.exists(file.path(library, "toast")))
  for (worker in workers)
    expect_false(any(file.exists(c(worker$script, worker$data))))

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

})

test_that("binary worker processes preserve custom copy method closures", {

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

  library <- renv_libpaths_active()
  prepared <- renv_graph_install_prepare(list(Package = "bread"), source, library, "binary", FALSE)
  server <- renv_socket_server()
  defer(close(server$socket))
  worker <- renv_graph_install_launch_socket(prepared, server$port)
  defer(renv_graph_install_worker_cleanup(worker))
  conn <- renv_socket_accept(server$socket, open = "rb", timeout = 30)
  defer(close(conn))

  expect_identical(unserialize(conn), "bread")
  expect_identical(unserialize(conn), character())
  expect_equal(readLines(file.path(library, "bread", "sentinel")), "captured by the copy method")

})
