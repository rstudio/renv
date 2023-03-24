
`_renv_packages_base` <- NULL
`_renv_packages_recommended` <- NULL

renv_packages_base <- function() {

  `_renv_packages_base` <<- `_renv_packages_base` %||% {
    db <- installed_packages(lib.loc = .Library, priority = "base")
    c("R", db$Package, "translations")
  }

}

renv_packages_recommended <- function() {

  `_renv_packages_recommended` <<- `_renv_packages_recommended` %||% {
    db <- installed_packages(lib.loc = .Library, priority = "recommended")
    db$Package
  }

}
