
# tools for querying information about packages available on CRAN
available_packages <- function(type) {
  renv_timecache(
    list(repos = getOption("repos"), type = type),
    renv_available_packages_impl(type)
  )
}

renv_available_packages_impl <- function(type) {

  # notify user since this can take some time
  fmt <- "* Querying repositories for available %s packages -- please wait a moment ..."
  messagef(fmt, type)

  # force a CRAN mirror when needed
  repos <- getOption("repos") %||% character()
  repos[repos == "@CRAN@"] <- "https://cran.rstudio.com"
  options(repos = repos)

  # request packages
  ap <- tryCatch(available.packages(type = type), error = function(e) NULL)
  as.data.frame(ap, stringsAsFactors = FALSE)

}
