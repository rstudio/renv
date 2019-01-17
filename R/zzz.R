
.onLoad <- function(libname, pkgname) {

  # save the default values seen for environment variables that we mutate
  # when activating / deactivating a virtual environment
  Sys.setenv(

    RENV_DEFAULT_LIBPATHS       = paste(renv_libpaths_all(), collapse = .Platform$path.sep),

    RENV_DEFAULT_R_PROFILE      = Sys.getenv("R_PROFILE"),
    RENV_DEFAULT_R_PROFILE_USER = Sys.getenv("R_PROFILE_USER"),

    RENV_DEFAULT_R_ENVIRON      = Sys.getenv("R_ENVIRON"),
    RENV_DEFAULT_R_ENVIRON_USER = Sys.getenv("R_ENVIRON_USER"),

    RENV_DEFAULT_R_LIBS_USER    = Sys.getenv("R_LIBS_USER"),
    RENV_DEFAULT_R_LIBS_SITE    = Sys.getenv("R_LIBS_SITE"),
    RENV_DEFAULT_R_LIBS         = Sys.getenv("R_LIBS"),

    RENV_DEFAULT_SYSTEM_LIBRARY = paste(.Library, collapse = .Platform$path.sep),
    RENV_DEFAULT_SITE_LIBRARY   = paste(.Library.site, collapse = .Platform$path.sep)

  )

  # copy our cached repositories to the R tempdir so that they might be
  # re-used without forcing extra queries to CRAN
  cache <- renv_paths_repos()
  sources <- list.files(cache, full.names = TRUE)
  targets <- file.path(tempdir(), sprintf("repos_%s", basename(sources)))
  mapply(function(source, target) {
    if (!file.exists(target))
      renv_file_link(source, target)
  }, sources, targets)

}
