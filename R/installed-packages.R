
installed_packages <- function(lib.loc = NULL,
                               priority = NULL,
                               field = NULL)
{
  lib.loc <- lib.loc %||% .libPaths()

  result <- dynamic(
    key = list(lib.loc = lib.loc, priority = priority),
    value = {
      packages <- installed.packages(lib.loc = lib.loc, priority = priority)
      as_data_frame(packages)
    }
  )

  take(result, field)

}
