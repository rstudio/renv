
installed_packages <- function(lib.loc = NULL, priority = NULL) {

  lib.loc <- lib.loc %??% .libPaths()

  dynamic(
    key = list(lib.loc = lib.loc, priority = priority),
    value = {
      packages <- installed.packages(lib.loc = lib.loc, priority = priority)
      as_data_frame(packages)
    }
  )

}
