
renv_package_installed <- function(package, lib.loc = NULL) {
  location <- find.package(package, lib.loc = lib.loc, quiet = TRUE)
  length(location) > 0
}

renv_package_version <- function(package) {
  renv_package_description_field(package, "Version")
}

renv_package_description_field <- function(package, field) {
  desc <- renv_description_read(package = package)
  desc[[field]]
}

renv_package_type <- function(path) {

  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("no package at path '%s'", aliased_path(path))

  # for directories, assume it's a source package
  if (info$isdir)
    return("source")

  # otherwise, guess based on contents of package
  methods <- list(
    function(path) untar(tarfile = path, list = TRUE),
    function(path) unzip(zipfile = path, list = TRUE)$Name
  )

  for (method in methods) {

    files <- catch(method(path))
    if (inherits(files, "error"))
      next

    if (any(grepl("^[^/]+/Meta/?$", files)))
      return("binary")
    else
      return("source")

  }

  # all else fails, assume source
  fmt <- "failed to determine type of package '%s'; assuming source"
  warningf(fmt, aliased_path(path))

  "source"

}

renv_package_priority <- function(package) {

  # treat 'R' as pseudo base package
  if (package == "R")
    return("base")

  # read priority from db
  db <- renv_installed_packages()
  entry <- db[package, ]
  entry$Priority %NA% ""

}

renv_package_tarball_name <- function(path) {
  desc <- renv_description_read(path)
  with(desc, sprintf("%s_%s.tar.gz", Package, Version))
}

renv_package_ext <- function(type) {

  type <- match.arg(type, c("binary", "source"))
  if (type == "source")
    return(".tar.gz")

  switch(
    Sys.info()[["sysname"]],
    Darwin  = ".tgz",
    Windows = ".zip",
    ".tar.gz"
  )

}

renv_packages_base <- function() {
  db <- renv_installed_packages_base()
  c("R", db$Package)
}

renv_package_pkgtypes <- function() {

  # only use binaries if the user has specifically requested it
  # and binaries are available for this installation of R
  # (users may want to install from sources explicitly to take
  # advantage of custom local compiler configurations)
  binaries <-
    !identical(.Platform$pkgType, "source") &&
    !identical(getOption("pkgType"), "source")

  if (binaries) c("binary", "source") else "source"

}
