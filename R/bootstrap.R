
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
  target <- renv_paths_bootstrap("renv", renv_package_version("renv"), "renv")

  # handle attempts to re-bootstrap renv
  if (renv_file_same(source, target))
    return(TRUE)

  # copy the directory
  unlink(target, recursive = TRUE)
  ensure_parent_directory(target)
  renv_file_copy(source, target)

}
