
renv_manifest_write <- function(manifest, file = stdout()) {
  renv_manifest_write_begin(file)
  on.exit(renv_manifest_write_end(), add = TRUE)
  renv_manifest_write_list(manifest, section = character())
  invisible(manifest)
}

renv_manifest_write_list <- function(entry, section) {
  enumerate(entry, renv_manifest_write_atoms, section = section)
  enumerate(entry, renv_manifest_write_lists, section = section)
}

renv_manifest_write_atoms <- function(key, value, section) {

  sublists <- map_lgl(value, function(x) identical(class(x), "list"))
  if (all(sublists))
    return()

  subsection <- c(section, key)
  label <- sprintf("[%s]", paste(subsection, collapse = "."))
  renv_manifest_write_emit(label)

  enumerate(value[!sublists], renv_manifest_write_atom)
  renv_manifest_write_emit()

}

renv_manifest_write_atom <- function(key, value) {

  lhs <- key
  rhs <- if (is_named(value))
    sprintf("\n\t%s=%s", names(value), value)
  else
    paste(value, collapse = ", ")

  text <- paste(lhs, rhs, sep = "=")
  renv_manifest_write_emit(text)

}

renv_manifest_write_lists <- function(key, value, section) {
  sublists <- map_lgl(value, function(x) identical(class(x), "list"))
  renv_manifest_write_list(value[sublists], section = c(section, key))
}

renv_manifest_write_begin <- function(file) {
  file <- if (is.character(file)) textfile(file) else file
  renv_global_set("manifest.file", file)
}

renv_manifest_write_end <- function() {
  file <- renv_global_get("manifest.file")
  if (inherits(file, "file"))
    close(file)
  renv_global_clear("manifest.file")
}

renv_manifest_write_emit <- function(text = "") {
  writeLines(text, con = renv_global_get("manifest.file"))
}
