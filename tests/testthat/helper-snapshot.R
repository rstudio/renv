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

  x <- gsub(normalizePath(getwd()), "<wd>", x, fixed = TRUE)

  x <- gsub(renv_paths_cache(), "<cache>", x, fixed = TRUE)
  x <- gsub(Sys.getenv("RENV_PATHS_ROOT"), "<root>", x, fixed = TRUE)
  x <- gsub(getOption("repos")[[1]], "<test-repo>", x, fixed = TRUE)
  x <- gsub(renv_path_normalize(tempdir()), "<tempdir>", x, fixed = TRUE)

  x <- gsub("renv-library-\\w+", "<renv-library>", x)

  x <- gsub(getRversion(), "<r-version>", x, fixed = TRUE)
  x <- gsub(renv_platform_prefix(), "<platform-prefix>", x, fixed = TRUE)
  x
}
