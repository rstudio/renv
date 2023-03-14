
`_renv_packages_base` <- NULL

renv_packages_base <- function() {

  `_renv_packages_base` <<- `_renv_packages_base` %||% {
    db <- installed_packages(library = .Library, priority = "base")
    c("R", db$Package, "translations")
  }

}
