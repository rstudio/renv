#' @inheritParams renv-params
#' @export
renv_restore <- function(manifest, confirm = interactive())
{
  if (is.character(manifest))
    manifest <- renv_manifest_read(manifest)

  libpath <- renv_paths_library(library)
  ensure_directory(libpath)

  libpaths <- .libPaths()
  .libPaths(c(libpath, libpaths))
  on.exit(.libPaths(libpaths), add = TRUE)

  # TODO: 'diff' the manifest against the current state of
  # the environment installed on the system


  actions <- manifest_diff()
  actions <- lapply(manifest, renv_restore_actions_create)
  if (confirm)
    renv_restore_actions_report(actions)
  renv_restore_actions_execute(actions)

}

renv_restore_package <- function(entry) {
  # TODO: Attempt installation from a local cache of installed packages.
  # TODO: Check for GitHub etc.
  renv_restore_package_cran(entry)

}

renv_restore_package_cran <- function(entry) {

  # TODO: Allow installation of binary rather than source package.
  # TODO: 'pkgType' should be a project-local option.
  # TODO: Cache 'available.packages' state since it's expensive to query.
  ap <- utils::available.packages()


}
