
available_packages <- function(type) {
  # TODO: maintain local in-memory cache of available packages?
  # need to make sure we respect both repos + type
  ap <- available.packages(type = type)
  as.data.frame(ap, stringsAsFactors = FALSE)
}
