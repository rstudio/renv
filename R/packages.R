
the$packages_base <- NULL
the$packages_recommended <- NULL

renv_packages_base <- function() {

  the$packages_base <- the$packages_base %||% {
    db <- installed_packages(lib.loc = .Library, priority = "base")
    c("R", db$Package, "translations")
  }

}
