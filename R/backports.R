
if (getRversion() < "3.2.0") {
  dir.exists <- function(paths) {
    info <- file.info(paths, extra_cols = FALSE)
    info$isdir %in% TRUE
  }
}
