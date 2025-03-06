
the$lockfile_state <- new.env(parent = emptyenv())

renv_lockfile_state_get <- function(key) {
  if (exists(key, envir = the$lockfile_state))
    get(key, envir = the$lockfile_state, inherits = FALSE)
}

renv_lockfile_state_set <- function(key, value) {
  assign(key, value, envir = the$lockfile_state, inherits = FALSE)
}

renv_lockfile_state_clear <- function() {
  rm(list = ls(the$lockfile_state), envir = the$lockfile_state)
}

renv_lockfile_write_preflight <- function(old, new) {

  diff <- renv_lockfile_diff(old, new)
  if (empty(diff))
    return(new)

  packages <- diff$Packages
  if (empty(diff$Packages))
    return(new)

  enumerate(packages, function(package, changes) {

    # avoid spurious changes
    lhs <- "CRAN"
    for (rhs in c("RSPM", "PPM", "P3M")) {

      spurious <-
        identical(changes, list(Repository = list(before = lhs, after = rhs))) ||
        identical(changes, list(Repository = list(before = rhs, after = lhs)))

      if (spurious)
        new$Packages[[package]]$Repository <<- old$Packages[[package]]$Repository

    }

  })

  new

}

renv_lockfile_write <- function(lockfile, file = stdout()) {

  # if we're updating an existing lockfile, try to avoid
  # "unnecessary" diffs that might otherwise be annoying
  if (is.character(file) && file.exists(file)) {
    old <- catch(renv_lockfile_read(file))
    if (!inherits(old, "error"))
      lockfile <- renv_lockfile_write_preflight(old, lockfile)
  }

  lockfile <- renv_lockfile_sort(lockfile)
  result <- renv_lockfile_write_json(lockfile, file)

  if (is.character(file))
    writef("- Lockfile written to %s.", renv_path_pretty(file))

  result

}

renv_lockfile_write_json_prepare_repos <- function(repos) {

  prepared <- enumerate(repos, function(name, url) {
    url <- sub("/+$", "", url)
    list(Name = name, URL = url)
  })

  unname(prepared)
}

renv_lockfile_write_json_prepare <- function(key, val) {

  if (key == "Repositories")
    renv_lockfile_write_json_prepare_repos(val)
  else if (is.list(val) && !is.null(names(val)))
    enumerate(val, renv_lockfile_write_json_prepare)
  else
    val

}

renv_lockfile_write_json <- function(lockfile, file = stdout()) {

  prepared <- enumerate(lockfile, renv_lockfile_write_json_prepare)

  box <- c("Requirements")
  config <- list(box = box)
  
  json <- renv_json_convert(prepared, config)
  if (is.null(file))
    return(json)

  writeLines(json, con = file)

}

renv_lockfile_write_internal <- function(lockfile,
                                         file = stdout(),
                                         delim = "=")
{
  if (is.character(file)) {
    file <- textfile(file)
    defer(close(file))
  }

  emitter <- function(text) writeLines(text, con = file)

  renv_lockfile_state_set("delim", delim)
  renv_lockfile_state_set("emitter", emitter)
  defer(renv_lockfile_state_clear())

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

  delim <- renv_lockfile_state_get("delim")
  text <- paste(lhs, rhs, sep = delim)
  renv_lockfile_write_emit(text)

}

renv_lockfile_write_lists <- function(key, value, section) {
  sublists <- map_lgl(value, function(x) identical(class(x), "list"))
  renv_lockfile_write_list(value[sublists], section = c(section, key))
}

renv_lockfile_write_emit <- function(text = "") {
  emitter <- renv_lockfile_state_get("emitter")
  emitter(text)
}
