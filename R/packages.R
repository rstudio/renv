
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
    untar = function(path) untar(tarfile = path, list = TRUE),
    unzip = function(path) unzip(zipfile = path, list = TRUE)$Name
  )

  for (method in methods) {

    files <- catch(method(path))
    if (inherits(files, "error"))
      next

    if (any(grepl("^[^/]+/INDEX$", files)))
      return("binary")
    else
      return("source")

  }

  # all else fails, assume source
  fmt <- "failed to determine type of package '%s'; assuming source"
  warningf(fmt, aliased_path(path))

  "source"

}
