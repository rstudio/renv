
#' Generate `renv.lock` from an RStudio Connect `manifest.json`
#'
#' Use `renv_lockfile_from_manifest()` to convert a `manifest.json` file from
#' an RStudio Connect content bundle into an `renv.lock` lockfile.
#'
#' This function can be useful when you need to recreate the package environment
#' of a piece of content that is deployed to RStudio Connect. The content bundle
#' contains a `manifest.json` file that is used to recreate the package
#' environment. This function will let you convert that manifest file to an
#' `renv.lock` file. Run `renv::restore()` after you've converted the file to
#' restore the package environment.
#'
#' @param manifest
#'   The path to a `manifest.json` file.
#'
#' @param lockfile
#'   The path to the lockfile to be generated and / or updated.
#'   When `NA` (the default), the generated lockfile is returned as an \R
#'   object; otherwise, the lockfile will be written to the path specified by
#'   `lockfile`.
#'
#' @details
#' By default the `lockfile` argument is set to `NA`. This will not create a new
#' `renv.lock` file. Rather, it will return a lockfile object (see `?lockfile`)
#' that can be used to create a new `renv.lock` file. If `lockfile` is set to a
#' character string, a new file will be created with that path -- e.g.
#' `renv.lock` -- and the lockfile object will be returned.
#'
#' @return
#' An `renv` lockfile.
#'
#' @keywords internal
renv_lockfile_from_manifest <- function(manifest,
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
  version <- numeric_version(manifest[["platform"]] %||% getRversion())

  # create R field for lockfile
  r <- list(Version = version, Repositories = repos)

  # create the lockfile
  lock <- list(R = r, Packages = records)
  class(lock) <- "renv_lockfile"

  # return lockfile as R object if requested
  if (is.na(lockfile))
    return(lock)

  # otherwise, write to file and report for user
  renv_lockfile_write(lock, file = lockfile)
  fmt <- "* Lockfile written to %s."
  vwritef(fmt, renv_path_pretty(lockfile))

  invisible(lock)

}
