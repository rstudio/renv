
# Is 'data' (a parsed JSON object) a Posit Connect manifest? Connect manifests
# carry lowercase 'metadata' and 'packages' fields, which lets us distinguish
# them from an renv lockfile (which uses capitalized 'R', 'Packages', etc.).
renv_manifest_is <- function(data) {
  is.list(data) &&
    is.list(data[["metadata"]]) &&
    is.list(data[["packages"]])
}

# Convert a Posit Connect 'manifest.json' (either a path, or an already-read
# manifest as an R list) into an renv lockfile. Driven by `lockfile()`.
renv_lockfile_from_manifest <- function(manifest = "manifest.json",
                                        lockfile = NA,
                                        project = NULL)
{
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  # read the manifest (accept both lists and file paths)
  manifest <- case(
    is.character(manifest) ~ renv_json_read(manifest),
    is.list(manifest)      ~ manifest,
    TRUE                   ~ renv_type_unexpected(manifest)
  )

  # convert descriptions into records
  records <- map(manifest[["packages"]], function(entry) {
    desc <- entry[["description"]]
    renv_snapshot_description_impl(desc)
  })

  # extract repositories from descriptions
  repos <- list()
  for (entry in manifest[["packages"]]) {

    if (is.null(entry[["Repository"]]))
      next

    src <- entry[["Source"]] %||% "CRAN"
    repo <- entry[["Repository"]]

    repos[[src]] <- repo

  }

  # extract version
  version <- format(manifest[["platform"]] %||% getRversion())

  # create R field for lockfile
  r <- list(Version = version, Repositories = repos)

  # create the lockfile
  lock <- list(R = r, Packages = records)
  class(lock) <- "renv_lockfile"

  # return lockfile as R object if requested
  if (is.na(lockfile))
    return(lock)

  # otherwise, write to file
  renv_lockfile_write(lock, file = lockfile)

  invisible(lock)

}
