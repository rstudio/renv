
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

# a stand-in for the record renv_remotes_resolve() would produce for the
# sources above, so that vendor() needn't reach GitHub during tests
renv_tests_vendor_remote <- function(sources) {

  desc <- renv_description_read(file.path(sources, "DESCRIPTION"))

  list(
    Package        = "renv",
    Version        = desc[["Version"]],
    Source         = "GitHub",
    RemoteType     = "github",
    RemoteHost     = "api.github.com",
    RemoteUsername = "rstudio",
    RemoteRepo     = "renv",
    RemoteSha      = desc[["RemoteSha"]] %||% strrep("0", 40L)
  )

}
