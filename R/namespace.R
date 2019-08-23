
renv_namespace_spec <- function(package) {
  namespace <- asNamespace(package)
  .getNamespaceInfo(namespace, "spec")
}

renv_namespace_version <- function(package) {
  spec <- renv_namespace_spec(package)
  spec[["version"]]
}
