
# the sources for the version of renv under test: the source checkout when
# running under devtools, or the sources extracted by R CMD check. NULL if
# neither is available, in which case vendor() downloads sources from GitHub
renv_tests_vendor_sources <- function() {

  nspath <- getNamespaceInfo(asNamespace("renv"), "path")
  candidates <- c(nspath, file.path(dirname(nspath), "00_pkg_src", "renv"))

  for (candidate in candidates)
    if (file.exists(file.path(candidate, "R", "vendor.R")))
      return(candidate)

  NULL

}
