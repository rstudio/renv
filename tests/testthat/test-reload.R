
test_that("renv can be reloaded within the same session", {
  skip_on_cran()

  script <- renv_test_code({
    renv:::summon()

    # set up temporary library path
    libdir <- tempfile("renv-library-")
    ensure_directory(libdir)
    .libPaths(libdir)

    # install CRAN version of renv
    options(warn = 2L)
    options(repos = c(CRAN = "https://cloud.R-project.org"))
    install("renv@1.0.0")

    # unload and reload renv
    unloadNamespace("renv")
    loadNamespace("renv")

    # try writing out some data
    writeLines(paste(renv_package_version("renv"), renv_namespace_version("renv")))

  })

  output <- renv_system_exec(
    command = R(),
    args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    action = "reloading renv"
  )

  expect_equal(tail(output, n = 1L), "1.0.0 1.0.0")

})
