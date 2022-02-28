
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

renv_namespace_load <- function(package) {
  suppressPackageStartupMessages(getNamespace(package))
}

renv_namespace_unload <- function(package) {
  unloadNamespace(package)
}

renv_namespace_parse <- function(package) {

  parseNamespaceFile(
    package     = package,
    package.lib = dirname(renv_package_find(package)),
    mustExist   = TRUE
  )

}
