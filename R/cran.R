
available_packages <- function(type) {
  # TODO: maintain local in-memory cache of available packages?
  # need to make sure we respect both repos + type
  as.data.frame(available.packages(type = type), stringsAsFactors = FALSE)
}
