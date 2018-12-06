# TODO: once versions of 'renv' are available on CRAN we should make it
# possible to install and use a particular version of 'renv' if so desired;
# right now we just use whatever version the user happened to have in their
# library
renv_bootstrap <- function(version = package_version("renv"),
                           local = FALSE,
                           force = FALSE)
{
  path <- renv_paths_renv(local = local)

  # check for existing 'renv' source
  source <- find.package("renv", quiet = TRUE)
  if (!file.exists(source))
    stop("no installation of 'renv' detected locally")

  # check to see if we already have an installation of 'renv' available
  target <- file.path(path, sprintf("renv-%s", packageVersion("renv")))

  # TODO: later
  # if (file.exists(file.path(target, "renv")) && !force)
  #   return(TRUE)

  # copy the directory
  unlink(file.path(target, "renv"), recursive = TRUE)
  ensure_directory(target)
  file.copy(source, target, recursive = TRUE)

}
