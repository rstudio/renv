
renv_installed_packages_base <- function() {
  ip <- installed.packages(lib.loc = .Library, priority = "base")
}
