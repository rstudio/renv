
test_that("all ffi methods have matching formal definitions", {
  
  methods <- ls(envir = renv_envir_self(), pattern = "^__ffi__")
  map(methods, function(method) {
    lhs <- get(method, envir = renv_envir_self())
    rhs <- get(substring(method, 8L), envir = renv_envir_self())
    expect_identical(formals(lhs), formals(rhs))
  })
  
})

test_that("the renv extensions library was loaded if available", {
  
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
