
test_that("all ffi methods have matching formal definitions", {
  
  enumerate(the$ffi, function(name, value) {
    expect_identical(
      object   = formals(value),
      expected = formals(get(name, envir = renv_envir_self())),
      info     = name
    )
  })
  
})
