
.onLoad <- function(libname, pkgname) {

  # cache the path to the actually-installed 'renv'
  renv_global_set("renv", renv_package_find("renv"))

  # install renv shims
  renv_shims_init()

  # copy our cached repositories to the R tempdir so that they might be
  # re-used without forcing extra queries to CRAN
  cache <- renv_paths_repos()
  sources <- list.files(cache, full.names = TRUE)
  targets <- file.path(tempdir(), sprintf("repos_%s", basename(sources)))
  mapply(function(source, target) {
    if (!renv_file_exists(target))
      renv_file_link(source, target)
  }, sources, targets)

}
