
expect_snapshot <- function(...,
                            cran = FALSE,
                            error = FALSE,
                            transform = strip_dirs,
                            variant = NULL,
                            cnd_class = FALSE)
{
  renv_scope_options(renv.verbose = TRUE)
  testthat::expect_snapshot(
    ...,
    cran = cran,
    error = error,
    transform = transform,
    variant = variant,
    cnd_class = cnd_class
  )
}

strip_dirs <- function(x) {

  # TODO: we might want to map multiple strings to the same
  # placeholder, so this should probably be flipped?
  #
  # note also that order matters for snapshot tests; the least-specific
  # paths should go at the end of this list
  filters <- list(
    "<R>"               = file.path(R.home("bin"), "R"),
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
  x <- gsub(renv_path_aliased(getwd()), "<wd>", x, fixed = TRUE)
  x <- gsub(renv_path_aliased(tempdir()), "<tempdir>", x, fixed = TRUE)

  # Standardise the dashes produced by header()
  x <- gsub("-{3,}\\s*$", "---", x, perl = TRUE)

  # Standardise version
  x <- gsub(renv_metadata_version_friendly(), "<version>", x, fixed = TRUE)

  x

}

strip_versions <- function(x) {
  gsub("\\[[0-9.-]*\\]", "[<version>]", x)
}
