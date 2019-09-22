
renv_package_find <- function(package, libpaths = renv_libpaths_all()) {

  # first, look in the library paths (specifically lib.loc so that we avoid
  # using the path to loaded namespaces)
  location <- renv_package_find_impl(package, libpaths)
  if (file.exists(location))
    return(location)

  # if that failed, check to see if it's loaded and use the associated path
  if (package %in% loadedNamespaces()) {
    path <- renv_namespace_path(package)
    if (file.exists(path))
      return(path)
  }

  # try looking in the cache for the package
  location <- renv_paths_cache(package)
  versions <- list.files(location)
  if (!length(versions))
    return("")

  # take the most recent version
  sorted <- versions[order(numeric_version(versions))]
  version <- tail(sorted, n = 1)

  # now, check and see how many installations are associated with
  # this version
  hashes <- list.files(file.path(location, version))
  if (length(hashes) == 0)
    return("")

  # TODO: if we have multiple versions of the package hashed,
  # is there a way we can have the user request one or the other?
  file.path(location, version, hashes[[1]], package)

}

renv_package_find_impl <- function(packages, libpaths = renv_libpaths_all()) {
  map_chr(packages, renv_package_find_impl_one, libpaths = libpaths)
}

renv_package_find_impl_one <- function(package, libpaths) {

  # check for DESCRIPTION file for each library path
  for (libpath in libpaths) {
    pkgpath <- file.path(libpath, package)
    descpath <- file.path(pkgpath, "DESCRIPTION")
    if (file.exists(descpath))
      return(pkgpath)
  }

  # nothing found; return empty string
  ""

}
