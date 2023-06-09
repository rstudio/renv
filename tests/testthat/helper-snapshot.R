expect_snapshot <- function(..., transform = strip_dirs) {
  renv_scope_options(renv.verbose = TRUE)

  testthat::expect_snapshot(..., transform = transform)
}

strip_dirs <- function(x) {

  # TODO: we might want to map multiple strings to the same
  # placeholder, so this should probably be flipped?
  #
  # note also that order matters for snapshot tests; the least-specific
  # paths should go at the end of this list
  filters <- list(
    "<cache>"           = renv_paths_cache(),
    "<platform-prefix>" = renv_platform_prefix(),
    "<r-version>"       = getRversion(),
    "<test-repo>"       = getOption("repos")[[1L]],
    "<root>"            = renv_path_normalize(renv_paths_root()),
    "<wd>"              = renv_path_normalize(getwd()),
    "<tempdir>"         = renv_path_normalize(tempdir())
  )

  # apply filters
  enumerate(filters, function(target, source) {
    x <<- gsub(source, target, x, fixed = TRUE)
  })

  # other pattern-based filters here
  x <- gsub("renv-library-\\w+", "<renv-library>", x)

  x

}
