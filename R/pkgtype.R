
renv_pkgtype_check <- function(type) {

  case(
    type == "source" ~ renv_pkgtype_check_source(type),
    type == "binary" ~ renv_pkgtype_check_binary(type),
    ~ abort(sprintf("unrecognized type '%s'", type))
  )

}

renv_pkgtype_check_source <- function(type) {
  type
}

renv_pkgtype_check_binary <- function(type) {

  # if the user has requested installation of a binary package,
  # and this edition of R was compiled with support for binary
  # packages, then proceed
  pkgtype <- .Platform$pkgType
  if (grepl("\\bbinary\\b", pkgtype, perl = TRUE))
    return(type)

  # user has requested installation of binary packages, but their
  # installation of R only supports source packages
  abort("type 'binary' is not supported on this platform")

}
