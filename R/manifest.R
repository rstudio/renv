renv_manifest_read <- function(file) {

  contents <- read(file)
  parts <- strsplit(contents, "\n{2,}")[[1]]

  config <- renv_config_read(text = parts[[1]])
  parts <- utils::tail(parts, n = -1L)

  library <- lapply(parts, function(part) {
    pieces <- strsplit(part, "\n", fixed = TRUE)[[1]]
    idx <- regexpr(":", pieces, fixed = TRUE)
    named(
      trimws(substring(pieces, idx + 1)),
      trimws(substring(pieces, 1, idx - 1))
    )
  })

  list(config = config, library = library)

}

renv_manifest_write <- function(manifest, file) {

  header <- renv_manifest_serialize_config(manifest)
  body <- renv_manifest_serialize_library(manifest)
  contents <- paste(header, body, sep = "\n\n")
  write_lines(contents, con = file)

}

renv_manifest_serialize_config <- function(manifest) {
  renv_config_write(manifest$config, path = NULL, comment = FALSE)
}

renv_manifest_serialize_library <- function(manifest) {
  body <- enumerate(manifest$library, renv_manifest_serialize_library_one)
  paste(body, collapse = "\n\n")
}

renv_manifest_serialize_library_one <- function(library, entries) {
  paste(vapply(entries, function(entry) {
    entry[["Library"]] <- library
    paste(names(entry), entry, sep = ": ", collapse = "\n")
  }, character(1)), collapse = "\n\n")
}

