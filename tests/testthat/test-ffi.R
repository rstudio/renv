
test_that("all ffi methods have matching formal definitions", {

  methods <- ls(envir = renv_envir_self(), pattern = "^__ffi__")
  map(methods, function(method) {
    lhs <- get(method, envir = renv_envir_self())
    rhs <- get(substring(method, 8L), envir = renv_envir_self())
    expect_identical(formals(lhs), formals(rhs))
  })

})

test_that("the renv extensions library was loaded if available", {

  skip_if_not(renv_ext_enabled())

  # get path to shared library
  nspath <- renv_namespace_path("renv")
  soname <- if (renv_platform_windows()) "renv.dll" else "renv.so"
  libsdir <- renv_package_libsdir(nspath)
  sopath <- file.path(libsdir, soname)

  # if it exists, it should be loaded
  if (file.exists(sopath)) {

    dllinfo <- find(library.dynam(), function(dllinfo) {
      if (identical(dllinfo[["name"]], "renv"))
        return(dllinfo)
    })

    expect_true(renv_path_same(sopath, dllinfo[["path"]]))

  }

})

test_that("renv falls back to R implementations if extensions can't be loaded", {

  skip_if_not(renv_ext_enabled())
  renv_scope_envvars(DEVTOOLS_LOAD = NULL)

  # create a library containing an invalid shared object
  libname <- renv_scope_tempfile("renv-library-")
  arch <- if (nzchar(.Platform$r_arch)) .Platform$r_arch
  libsdir <- paste(c(libname, "renv", "libs", arch), collapse = "/")
  ensure_directory(libsdir)
  writeLines(c("Package: renv", "Version: 1.0.0"), file.path(libname, "renv/DESCRIPTION"))
  writeLines("not a shared library", file.path(libsdir, paste0("renv", .Platform$dynlib.ext)))

  # preserve the current dll info
  dll_info <- the$dll_info
  defer(the$dll_info <- dll_info)

  expect_warning(renv_ext_onload(libname, "renv"), "could not be loaded")
  expect_null(the$dll_info)

})

test_that("renv finds its extensions when the package directory isn't named 'renv'", {

  skip_if_not(renv_ext_enabled())
  renv_scope_envvars(DEVTOOLS_LOAD = NULL)

  # there has to be a shared library to load
  nspath <- renv_namespace_path("renv")
  soname <- if (renv_platform_windows()) "renv.dll" else "renv.so"
  sopath <- file.path(renv_package_libsdir(nspath), soname)
  skip_if_not(file.exists(sopath))

  # preserve the current dll info, and clear it so that an early return
  # (nothing found under 'libname') is detected
  dll_info <- the$dll_info
  defer(the$dll_info <- dll_info)
  the$dll_info <- NULL

  # a library that doesn't contain 'renv', as when the package lives in a
  # directory named after a git branch rather than after the package
  libname <- renv_scope_tempfile("renv-library-")
  ensure_directory(libname)

  expect_no_warning(renv_ext_onload(libname, "renv"))
  expect_true(renv_path_same(sopath, the$dll_info[["path"]]))

})
