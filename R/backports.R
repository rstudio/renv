
if (getRversion() < "3.2.0") {
  dir.exists <- function(paths) {
    info <- renv_file_info(paths)
    info$isdir %in% TRUE
  }
}
