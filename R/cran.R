
available_packages <- function(type) {
  renv_timecache(
    list(repos = getOption("repos"), type = type),
    renv_available_packages_impl(type)
  )
}

renv_available_packages_impl <- function(type) {
  ap <- available.packages(type = type)
  as.data.frame(ap, stringsAsFactors = FALSE)
}
