
# the minimum-required version of 'pak' for renv integration
the$pak_minver <- numeric_version("0.7.0")

renv_pak_init <- function(stream = NULL, force = FALSE) {

  stream <- stream %||% renv_pak_stream()
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
    if (version >= the$pak_minver)
      return(stream)
  }

  fmt <- "internal error: pak (>= %s) is not available"
  stopf(fmt, format(the$pak_minver))

}

renv_pak_available <- function() {
  tryCatch(
    packageVersion("pak") >= the$pak_minver,
    error = function(e) FALSE
  )
}

renv_pak_repos <- function(stream) {

  # on macOS, we can only use pak binaries with CRAN R
  if (renv_platform_macos() && .Platform$pkgType == "source")
    return(getOption("repos"))

  # otherwise, use pre-built pak binaries
  fmt <- "https://r-lib.github.io/p/pak/%s/%s/%s/%s"
  sprintf(fmt, stream, .Platform$pkgType, version$os, version$arch)

}

renv_pak_init_impl <- function(stream) {

  repos <- c("r-lib" = renv_pak_repos(stream))
  renv_scope_options(renv.config.pak.enabled = FALSE, repos = repos)

  library <- renv_libpaths_active()
  install("pak", library = library)
  loadNamespace("pak", lib.loc = library)

}

renv_pak_install <- function(packages, library, project) {

  pak <- renv_namespace_load("pak")
  lib <- library[[1L]]

  # transform repositories
  if (renv_ppm_enabled()) {
    repos <- getOption("repos")
    renv_scope_options(repos = renv_ppm_transform(repos))
  }

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

  # transform repositories
  if (renv_ppm_enabled()) {
    repos <- getOption("repos")
    renv_scope_options(repos = renv_ppm_transform(repos))
  }

  # make sure pak::pkg_install() still works even if we're
  # running in renv with devtools::load_all()
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    renv_scope_envvars("_R_CHECK_PACKAGE_NAME_" = NULL)

  # get records to install
  records <- renv_lockfile_records(lockfile)
  packages <- setdiff(packages %||% names(records), c(exclude, "pak", "renv"))
  records <- records[packages]

  # attempt to link packages that have cache entries
  if (renv_cache_config_enabled(project = project)) {
    linked <- map_lgl(records, renv_cache_synchronize)
    records <- records[!linked]
  }

  # convert into specs compatible with pak, and install
  remotes <- map_chr(records, renv_record_format_remote)

  # TODO: We previously tried converting version-ed remotes into "plain" remotes
  # if the package version happened to be current, but then 'pak' would choose
  # not to install the package if a newer version was available. Hence, we need
  # to preserve the exact remote we wish to install here.

  # return early if there are zero remotes to restore
  if (length(remotes) == 0L) {
    return(invisible(TRUE))
  }

  # perform installation
  pak$pkg_install(remotes)

  # return installed records
  records

}

