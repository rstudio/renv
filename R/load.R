renv_load_r_version <- function(config) {
  version <- config$r_version
  if (!is_compatible_version(version, getRversion())) {
    fmt <- "renv '%s' requested R version '%s' but '%s' is currently being used"
    warningf(fmt, renv_active_renv(), version, getRversion())
  }
}

renv_load_libpaths <- function(config, local) {

  renv <- file.path(renv_paths_renv(local = local), sprintf("renv-%s", config$renv_version))
  libs <- rev(renv_paths_lib(config$r_libs, local = local))
  lapply(libs, ensure_directory)

  libpaths <- c(libs, renv, if (config$r_libs_overlay) .libPaths())
  .libPaths(libpaths)

}

renv_load_repos <- function(config) {
  if (!empty(config$r_repos))
    options(repos = config$r_repos)
}
