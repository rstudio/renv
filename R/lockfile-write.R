
renv_lockfile_write <- function(lockfile, file = stdout()) {
  renv_lockfile_write_begin(file)
  on.exit(renv_lockfile_write_end(), add = TRUE)
  lockfile <- renv_lockfile_sort(lockfile)
  renv_lockfile_write_list(lockfile, section = character())
  invisible(lockfile)
}

renv_lockfile_write_list <- function(entry, section) {
  enumerate(entry, renv_lockfile_write_atoms, section = section)
  enumerate(entry, renv_lockfile_write_lists, section = section)
}

renv_lockfile_write_atoms <- function(key, value, section) {

  sublists <- map_lgl(value, function(x) identical(class(x), "list"))
  if (all(sublists))
    return()

  subsection <- c(section, key)
  label <- sprintf("[%s]", paste(subsection, collapse = "/"))
  renv_lockfile_write_emit(label)

  enumerate(value[!sublists], renv_lockfile_write_atom)
  renv_lockfile_write_emit()

}

renv_lockfile_write_atom <- function(key, value) {

  lhs <- key
  rhs <- if (is_named(value))
    paste(sprintf("\n\t%s=%s", names(value), value), collapse = "")
  else
    paste(value, collapse = ", ")

  text <- paste(lhs, rhs, sep = "=")
  renv_lockfile_write_emit(text)

}

renv_lockfile_write_lists <- function(key, value, section) {
  sublists <- map_lgl(value, function(x) identical(class(x), "list"))
  renv_lockfile_write_list(value[sublists], section = c(section, key))
}

renv_lockfile_write_begin <- function(file) {
  file <- if (is.character(file)) textfile(file) else file
  renv_global_set("lockfile", file)
}

renv_lockfile_write_end <- function() {
  file <- renv_global_get("lockfile")
  if (inherits(file, "file"))
    close(file)
  renv_global_clear("lockfile")
}

renv_lockfile_write_emit <- function(text = "") {
  writeLines(text, con = renv_global_get("lockfile"))
}
