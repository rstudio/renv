
#' Fork a Virtual Environment
#'
#' Fork a (global) virtual environment into a project-local one. This is useful
#' for isolating a project -- for example, if you need to preserve the current
#' state of a global environment locally, and make your own (persistent)
#' modifications to that environment without affecting the original environment.
#'
#' @inheritParams renv-params
#'
#' @export
fork <- function(name) {

  # TODO: we probably need to handle RENV_PATHS_LIBRARY and/or
  # RENV_PATHS_ROOT here

  # TODO: behavior if environment already exists?

  # restore local state when we're done
  local <- renv_state$local()
  on.exit(renv_state$local(local), add = TRUE)

  # get source, target blueprint paths
  renv_state$local(FALSE)
  source <- ensure_existing_renv(name)

  renv_state$local(TRUE)
  target <- renv_paths_environment(name)

  # copy the blueprint
  ensure_parent_directory(target)
  unlink(target)
  renv_file_copy(source, target)

  # copy over the renv R libraries (if any)
  blueprint <- renv_manifest_read(source)
  for (library in blueprint$R$Libraries) {
    renv_state$local(FALSE)
    libsource <- renv_paths_library(library)

    renv_state$local(TRUE)
    libtarget <- renv_paths_library(library)

    unlink(libtarget, recursive = TRUE)
    renv_file_copy(libsource, libtarget)
  }

  fmt <- lines(
    "* Successfully forked virtual environment '%1$s'. Activate it with:",
    "",
    "\trenv::activate(\"%1$s\", local = TRUE)",
    ""
  )
  vmessagef(fmt, name)

  target

}
