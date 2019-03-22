
renv_package_find <- function(package, libpaths = renv_libpaths_all()) {

  # first, look in the library paths (specifiy lib.loc so that we avoid
  # using the path to loaded namespaces)
  location <- find.package(package, quiet = TRUE, lib.loc = libpaths) %||% ""
  if (renv_file_exists(location))
    return(location)

  # if that failed, try looking in the cache for the latest
  # version of the package
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

  file.path(location, version, hashes[[1]], package)

}
