
.onLoad <- function(libname, pkgname) {

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

}
