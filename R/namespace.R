
renv_namespace_spec <- function(package) {
  namespace <- asNamespace(package)
  .getNamespaceInfo(namespace, "spec")
}

renv_namespace_version <- function(package) {
  spec <- renv_namespace_spec(package)
  spec[["version"]]
}

renv_namespace_path <- function(package) {
  namespace <- asNamespace(package)
  .getNamespaceInfo(namespace, "path")
}

# The library path providing `package`'s loaded namespace, or "" if the
# namespace was loaded from somewhere else. R records a namespace's path in
# fully resolved form, whereas a library entry is commonly a symlink (or, on
# Windows, a junction) into the renv cache -- so the two must be normalized
# before they can be compared.
# https://github.com/rstudio/renv/issues/2344
renv_namespace_libpath <- function(package, libpaths = renv_libpaths_all()) {

  nspath <- renv_namespace_path(package)

  # fast path for libraries which own their packages outright
  if (dirname(nspath) %in% libpaths)
    return(dirname(nspath))

  nspath <- renv_path_normalize(nspath)

  for (libpath in libpaths) {
    pkgpath <- file.path(libpath, package)
    if (file.exists(pkgpath) && identical(renv_path_normalize(pkgpath), nspath))
      return(libpath)
  }

  ""

}

renv_namespace_load <- function(package) {
  suppressPackageStartupMessages(getNamespace(package))
}

renv_namespace_unload <- function(package) {
  unloadNamespace(package)
}
