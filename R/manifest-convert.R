
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
renv_lockfile_from_manifest <- function(manifest, lockfile = NA) {

  renv_scope_error_handler()

  # if the user has provided a lockfile path, verify that it exists
  lockfile <- renv_type_check(lockfile, "character")
  if (!is.na(lockfile) && !file.exists(lockfile))
    file.create(lockfile)

  # read the manifest (accept both lists and file paths)
  manifest <- case(
    is.character(manifest) ~ renv_json_read(manifest),
    is.list(manifest)      ~ manifest,
    TRUE                   ~ renv_type_unexpected(manifest)
  )

  lock <- lockfile(lockfile)

  # read packages from manifest
  packages <- map(manifest[["packages"]], function(entry) {
    version <- entry[["description"]][["Version"]]
    package <- entry[["description"]][["Package"]]
    paste(package, version, sep = "@")
  })

  # read repositories from manifest
  sources <- map(manifest[["packages"]], function(entry) {
    source <- entry[["Source"]]
    names(source) <- entry[["Repository"]]
    source
  })

  repos <- as.list(unlist(unique(unname(sources))))

  # make updates
  lock$repos(.repos = repos)
  lock$add(.list = packages)
  lock$version(manifest[["platform"]])

  # return lockfile as R object if requested
  if (is.na(lockfile))
    return(lock)

  # otherwise, write to file
  lock$write(lockfile)

  # report to user
  fmt <- "* Lockfile written to %s."
  vwritef(fmt, renv_path_pretty(lockfile))

  invisible(lock)

}
