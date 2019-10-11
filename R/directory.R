
if (getRversion() < "3.2.0") {
  renv_directory_exists <- function(paths) {
    info <- file.info(paths, extra_cols = FALSE)
    info$isdir %in% TRUE
  }
} else {
  renv_directory_exists <- dir.exists
}
