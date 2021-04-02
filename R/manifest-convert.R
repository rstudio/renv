#' Convert manifest.json to an renv.lock file
#'
#' Use `rsc_manifest_to_renv_lock` to convert a `manifest.json` file from
#' an RStudio Connect content bundle into an `renv.lock` file.
#'
#' This function can be useful when you need to recreate the package environment
#' of a piece of content that is deployed to RStudio Connect. The content bundle
#' contains a `manifest.json` file that is used to recreate the package environment.
#' This function will let you convert that manifest file to an `renv.lock` file.
#' Run `renv::restore()` after you've converted the file to restore the package
#' environment.
#'
#' @param manifest The path to a manifest.json file.
#' @param lockfile The path to a lockfile you want to update, can be an empty text file. Defaults to NA.
#'
#' @details
#'
#' By default the `lockfile` argument is set to `NA`. This will not create a new renv.lock file.
#' Rather, it will return a lockfile object (see `?lockfile`) that can be used to create a new
#' renv.lock file. If `lockfile` is set to a character string, a new file will be created with that
#' path—e.g. `renv.lock`—and the lockfile object will be returned.
#'
#'
#' @export
rsc_manifest_to_renv_lock <- function(manifest = NULL, lockfile = NA) {

  if (missing(manifest)) {
    stop("Provide the path the manifest.json file.", call. = FALSE)
  }

  # create lockfile if non-existant and lockfile != NA
  if (!file.exists(lockfile %&&% "") & !is.na(lockfile)) {
    fmt <- "%s doesn't exist. Creating new file."
    vwritef(fmt, lockfile)
    file.create(lockfile)
  }

  cat("* Reading manifest.json...\n")
  .manifest <- renv_json_read(manifest)

  lock <- lockfile(lockfile)

  cat("* Parsing dependencies...\n")

  # packages
  manifest_pkgs <- lapply(.manifest[["packages"]], extract_pkg_version)

  # repos
  all_pkg_sources <- lapply(.manifest[["packages"]], extract_pkg_source)
  manifest_repos <- as.list(unlist(unique(unname(all_pkg_sources))))

  # make updates
  do.call(lock$repos, manifest_repos)
  do.call(lock$add, manifest_pkgs)
  lock$version(.manifest[["platform"]])

  if (is.na(lockfile)) {
    return(lock)
  } else{
    lock$write(lockfile)
    fmt <- "* %s written."
    vwritef(fmt, renv_path_pretty(lockfile))
  }

  invisible(lock)
}




# helper function for extracting the purl
extract_pkg_version <- function(manifest_pkg) {
  vrsn <- manifest_pkg[["description"]][["Version"]]
  name <- manifest_pkg[["description"]][["Package"]]
  paste0(name, "@", vrsn)
}

# package sources
extract_pkg_source <- function(manifest_pkg) {
  setNames(manifest_pkg[["Repository"]], manifest_pkg[["Source"]])
}
