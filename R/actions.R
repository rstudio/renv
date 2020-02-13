
actions <- function(action = c("snapshot", "restore"),
                    ...,
                    project = NULL,
                    library = NULL,
                    lockfile = file.path(project, "renv.lock"),
                    type = settings$snapshot.type(project = project),
                    clean = FALSE)
{
  action  <- match.arg(action)
  project <- renv_project_resolve(project)
  library <- library %||% renv_libpaths_all()

  switch(
    action,
    snapshot = renv_actions_snapshot(project, library, lockfile, type),
    restore  = renv_actions_restore(project, library, lockfile, clean)
  )
}

renv_actions_merge <- function(snap, lock, diff) {

  fields <- c("Package", "Version", "Source")
  defaults <- data.frame(
    "Package"          = character(),
    "Library Version"  = character(),
    "Library Source"   = character(),
    "Lockfile Version" = character(),
    "Lockfile Source"  = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  lhs <- bapply(unname(renv_records(snap)), `[`, fields)
  if (length(lhs))
    names(lhs) <- c("Package", paste("Library",  names(lhs)[-1L]))

  rhs <- bapply(unname(renv_records(lock)), `[`, fields)
  if (length(rhs))
    names(rhs) <- c("Package", paste("Lockfile", names(rhs)[-1L]))

  merged <- if (length(lhs) && length(rhs))
    merge(lhs, rhs, by = "Package", all = TRUE)
  else if (length(lhs))
    lhs
  else if (length(rhs))
    rhs
  else
    defaults

  actions <- data.frame(Package = names(diff),
                        Action = as.character(diff),
                        check.names = FALSE,
                        stringsAsFactors = FALSE)

  all <- merge(merged, actions, by = "Package")

  missing <- setdiff(names(defaults), names(all))
  all[missing] <- NA_character_

  all

}

renv_actions_snapshot <- function(project, library, lockfile, type) {

  lock <- renv_lockfile_load(project = project)
  snap <- snapshot(project = project,
                   library = library,
                   lockfile = NULL,
                   type = type)

  diff <- renv_lockfile_diff_packages(lock, snap)
  renv_actions_merge(snap, lock, diff)

}

renv_actions_restore <- function(project, library, lockfile, clean) {

  # NOTE: we use a simple snapshot here as we just want to know the
  # difference in library state before and after applying the lockfile;
  # that is, we want to know what the library looks like without any
  # filtering of what records would be reported from the library
  lock <- renv_lockfile_load(project = project)
  snap <- snapshot(project = project,
                   library = library,
                   lockfile = NULL,
                   type = "all")

  diff <- renv_lockfile_diff_packages(snap, lock)
  actions <- renv_actions_merge(snap, lock, diff)
  renv_actions_restore_clean(actions, clean, project)

}

renv_actions_restore_clean <- function(actions, clean, project) {

  # if not cleaning, then we don't do any removals
  if (!clean) {
    filtered <- actions[actions$Action != "remove", ]
    return(filtered)
  }

  # otherwise, only process removals in the project library
  projlib <- renv_paths_library(project = project)
  locations <- renv_package_find_impl(actions$Package)

  keep <- actions$Action != "remove" | dirname(locations) == projlib
  actions[keep, ]

}
