
available_packages <- function(type) {
  renv_timecache(
    list(repos = getOption("repos"), type = type),
    renv_available_packages_impl(type)
  )
}

renv_available_packages_impl <- function(type) {

  # force a CRAN mirror when needed
  if (renv_once()) {
    repos <- getOption("repos") %||% character()
    repos[repos == "@CRAN@"] <- "https://cran.rstudio.com"
    options(repos = repos)
  }

  # request packages
  ap <- available.packages(type = type)
  as.data.frame(ap, stringsAsFactors = FALSE)

}
