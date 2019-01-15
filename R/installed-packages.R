
renv_installed_packages_base <- function() {

  # we can assume that the base set of installed packages won't change during
  # a session, so cache the result of installed.packages()
  renv_global("base.packages", {
    ip <- installed.packages(lib.loc = .Library, priority = "base")
    as.data.frame(ip, stringsAsFactors = FALSE)
  })

}
