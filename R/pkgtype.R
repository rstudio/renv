
# Helpers for reasoning about R's package types, as reported by
# `.Platform$pkgType` and requested via the 'pkgType' option.
#
# R 4.6.0 generalized binary package types to the form
# '<system>.binary[.<build>]', mapping to repository paths of the form
# 'bin/<system>/<build>/contrib/<x.y>', and allowing binary packages to be
# distributed as .tar.bz2, .tar.xz, .tar.zst or .tar.zstd archives. The legacy
# 'win.binary' (.zip) and 'mac.binary[.<build>]' (.tgz) types remain in use
# by CRAN; R's own tooling classifies everything else as 'other.binary'.
# See tools:::.pkg.type() and utils::contrib.url().

# resolve the virtual "binary" type to the concrete type for this build of R
renv_pkgtype_resolve <- function(type = NULL) {

  type <- type %||% getOption("pkgType", default = "source")
  if (identical(type, "binary"))
    return(.Platform$pkgType)

  type

}

# classify a package type as one of "source", "win.binary", "mac.binary" or
# "other.binary", mirroring tools:::.pkg.type()
renv_pkgtype_class <- function(type = NULL) {

  type <- renv_pkgtype_resolve(type)
  if (!grepl(".binary", type, fixed = TRUE))
    return("source")

  system <- sub("^([[:lower:]]+)[.]binary.*$", "\\1", type)
  case(
    system == "win" ~ "win.binary",
    system == "mac" ~ "mac.binary",
    ~ "other.binary"
  )

}

# the archive extension R assumes for packages of this type when a repository's
# PACKAGES index carries no 'File' field; see utils::download.packages()
renv_pkgtype_ext <- function(type = NULL) {

  switch(
    renv_pkgtype_class(type),
    source       = ".tar.gz",
    win.binary   = ".zip",
    mac.binary   = ".tgz",
    other.binary = ".tar.xz"
  )

}

# the build designation 'R CMD INSTALL --build' embeds into binary archive
# names, as in 'pkg_1.0_R_<build>.tar.xz'. custom binary types produce
# '<system>-<build>' (e.g. 'macos-arm64'); R builds without a binary type of
# their own use the platform triplet; Windows and legacy macOS binaries have
# no designation at all
renv_pkgtype_build <- function(type = NULL) {

  type <- renv_pkgtype_resolve(type)
  class <- renv_pkgtype_class(type)

  if (class == "other.binary") {
    pattern <- "^([[:lower:]]+)[.]binary(|[.]([[:alnum:]_-]+))$"
    build <- sub(pattern, "\\1\\2", type)
    return(gsub(".", "-", build, fixed = TRUE))
  }

  if (class == "source" && renv_platform_unix())
    return(Sys.getenv("R_PLATFORM", unset = R.version$platform))

  NULL

}
