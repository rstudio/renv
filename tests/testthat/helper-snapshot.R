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
  x <- gsub(getwd(), "<wd>", x, fixed = TRUE)
  x <- gsub(renv_paths_cache(), "<cache>", x, fixed = TRUE)
  x <- gsub(getRversion(), "<r-version>", x, fixed = TRUE)
  x <- gsub(renv_platform_prefix(), "<platform-prefix>", x, fixed = TRUE)
  x
}
