
renv_packages_base <- function() {
  db <- renv_installed_packages_base()
  c("R", db$Package, "translations")
}

