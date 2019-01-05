
# installs 'renv' into a special bootstrap directory, which is then
# used by projects on startup so that they can load and use 'renv'
# without requiring a local install
renv_bootstrap <- function(force = FALSE) {

  # check for existing 'renv' source
  # TODO: use presently-loaded renv or not? or look up a version based
  # on a particular requested version?
  source <- find.package("renv", quiet = TRUE) %||% ""
  if (!file.exists(source))
    stop("no installation of 'renv' detected locally")

  # check to see if we already have an installation of 'renv' available
  target <- renv_paths_bootstrap("renv", renv_package_version("renv"))

  # TODO: later
  # if (file.exists(file.path(target, "renv")) && !force)
  #   return(TRUE)

  # copy the directory
  unlink(file.path(target, "renv"), recursive = TRUE)
  ensure_directory(target)
  file.copy(source, target, recursive = TRUE)

}
