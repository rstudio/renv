
`_renv_pak_version` <- numeric_version("0.4.0-9000")

renv_pak_init <- function(stream = NULL, force = FALSE) {

  stream <- stream %??% renv_pak_stream()
  if (force || !renv_pak_available())
    renv_pak_init_impl(stream)

  renv_namespace_load("pak")

}

renv_pak_stream <- function() {

  # check if stable is new enough
  streams <- c("stable", "rc", "devel")
  for (stream in streams) {
    repos <- renv_pak_repos(stream)
    latest <- renv_available_packages_latest("pak", repos = repos)
    version <- numeric_version(latest$Version)
    if (version >= `_renv_pak_version`)
      return(stream)
  }

  fmt <- "internal error: pak (>= %s) is not available"
  stopf(fmt, format(`_renv_pak_version`))

}

renv_pak_available <- function() {
  tryCatch(
    packageVersion("pak") >= `_renv_pak_version`,
    error = function(e) FALSE
  )
}

renv_pak_repos <- function(stream) {
  fmt <- "https://r-lib.github.io/p/pak/%s/%s/%s/%s"
  sprintf(fmt, stream, .Platform$pkgType, version$os, version$arch)
}

renv_pak_init_impl <- function(stream) {
  utils::install.packages("pak", repos = renv_pak_repos(stream))
}

renv_pak_install <- function(packages, library, project) {

  pak <- renv_namespace_load("pak")
  lib <- library[[1L]]

  # transform repositories
  if (renv_rspm_enabled())
    renv_scope_options(repos = renv_rspm_transform())

  # make sure pak::pkg_install() still works even if we're
  # running in renv with devtools::load_all()
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    renv_scope_envvars("_R_CHECK_PACKAGE_NAME_" = NULL)

  # if we received a named list of remotes, use the names
  packages <- if (any(nzchar(names(packages))))
     names(packages)
  else
    as.character(packages)

  if (length(packages) == 0L)
    return(pak$local_install_dev_deps(root = project, lib = lib))

  pak$pkg_install(
    pkg     = packages,
    lib     = lib,
    upgrade = TRUE
  )

}

renv_pak_restore <- function(lockfile,
                             packages = NULL,
                             exclude = NULL,
                             project = NULL)
{
  pak <- renv_namespace_load("pak")

  # make sure pak::pkg_install() still works even if we're
  # running in renv with devtools::load_all()
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    renv_scope_envvars("_R_CHECK_PACKAGE_NAME_" = NULL)

  # get records to install
  records <- renv_lockfile_records(lockfile)
  packages <- setdiff(packages %??% names(records), c(exclude, "pak", "renv"))
  records <- records[packages]

  # attempt to link packages that have cache entries
  if (renv_cache_config_enabled(project = project)) {
    linked <- map_lgl(records, renv_cache_synchronize)
    records <- records[!linked]
  }

  # convert into specs compatible with pak, and install
  remotes <- map_chr(records, renv_record_format_remote)

  # convert any remotes that happen to be current into plain package names
  types <- renv_package_pkgtypes()
  for (type in types) {
    dbs <- available_packages(type = type)
    for (db in dbs) {
      dbremotes <- paste(db$Package, db$Version, sep = "@")
      matches <- remotes %in% dbremotes
      remotes[matches] <- names(remotes[matches])
    }
  }

  # perform installation
  pak$pkg_install(remotes)
}

