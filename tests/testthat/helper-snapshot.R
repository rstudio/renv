expect_snapshot <- function(x, ...) {
  renv_scope_options(renv.verbose = TRUE)

  # This shouldn't be so hard
  eval.parent(
    substitute(
      testthat::expect_snapshot(x, ..., transform = strip_dirs)
    )
  )
}

strip_dirs <- function(x) {

  # TODO: we might want to map multiple strings to the same
  # placeholder, so this should probably be flipped
  filters <- list(
    "<cache>"          = renv_paths_cache(),
    "<platform-prefix>" = renv_platform_prefix(),
    "<r-version>"       = getRversion(),
    "<root>"            = renv_paths_root(),
    "<tempdir>"         = renv_path_normalize(tempdir()),
    "<test-repo>"       = getOption("repos")[[1L]],
    "<wd>"              = renv_path_normalize(getwd())
  )

  # apply filters
  enumerate(filters, function(target, source) {
    x <<- gsub(source, target, x, fixed = TRUE)
  })

  # other pattern-based filters here
  x <- gsub("renv-library-\\w+", "<renv-library>", x)

  x

}
