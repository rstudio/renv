
installed_packages <- function(lib.loc = NULL, priority = NULL, ...) {
  packages <- installed.packages(lib.loc = lib.loc, priority = priority, ...)
  as_data_frame(packages)
}
