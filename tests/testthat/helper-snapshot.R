
expect_snapshot <- function(...,
                            cran = FALSE,
                            error = FALSE,
                            transform = strip_dirs,
                            variant = NULL,
                            cnd_class = FALSE)
{
  if (renv_platform_windows() && getRversion() < "4.2") {
    testthat::skip("Snapshot tests unsupported on older Windows")
  }

  renv_scope_options(
    renv.caution.verbose = TRUE,
    renv.verbose = TRUE
  )

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
  prefix <- if (renv_platform_windows()) "^file:///" else "^file://"

  filters <- list(
    "<R>"               = file.path(R.home("bin"), "R"),
    "<cache>"           = renv_paths_cache(),
    "<platform-prefix>" = renv_platform_prefix(),
    "<r-version>"       = getRversion(),
    "<test-repo>"       = getOption("repos")[[1L]],
    "<test-repo-path>"  = gsub(prefix, "", getOption("repos")[[1L]]),
    "<root>"            = renv_path_normalize(renv_paths_root()),
    "<wd>"              = renv_path_aliased(getwd()),
    "<wd>"              = renv_path_normalize(getwd()),
    "<tempdir>"         = renv_path_normalize(tempdir()),
    "<wd-name>"         = basename(getwd())
  )

  # normalize backslashes to forward slashes so snapshots are
  # platform-independent; do this before applying filters so that
  # filter paths (which use forward slashes) match consistently
  x <- chartr("\\", "/", x)

  # apply filters
  enumerate(filters, function(target, source) {
    source <- chartr("\\", "/", source)
    x <<- gsub(source, target, x, fixed = TRUE)
  })

  # other pattern-based filters here
  x <- gsub("renv-library-\\w+", "<renv-library>", x)
  x <- gsub(renv_path_aliased(getwd()), "<wd>", x, fixed = TRUE)
  x <- gsub(renv_path_aliased(tempdir()), "<tempdir>", x, fixed = TRUE)

  # Standardise the dashes produced by header()
  x <- gsub("-{3,}\\s*$", "---", x, perl = TRUE)

  # Collapse width-dependent padding in progress lines
  x <- gsub("(\\.\\.\\.) +(OK|FAILED)", "\\1 \\2", x)

  # Standardise version
  x <- gsub(renv_metadata_version_friendly(), "<version>", x, fixed = TRUE)

  x

}

strip_versions <- function(x) {
  gsub("\\[[0-9.-]*\\]", "[<version>]", x)
}
