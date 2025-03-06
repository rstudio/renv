
# the minimum-required version of 'pak' for renv integration
the$pak_minver <- numeric_version("0.7.0")

renv_pak_init <- function(stream = NULL, force = FALSE) {

  if (force || !renv_pak_available()) {
    stream <- stream %||% renv_pak_stream()
    renv_pak_init_impl(stream)
  }

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

  renv_scope_options(
    renv.config.pak.enabled = FALSE,
    renv.config.ppm.enabled = FALSE,
    repos = c("r-lib" = renv_pak_repos(stream))
  )

  library <- renv_libpaths_active()
  install("pak", library = library)
  loadNamespace("pak", lib.loc = library)

}

renv_pak_update <- function(project, library, prompt) {
  
  pak <- renv_namespace_load("pak")

  # if this project contains a DESCRIPTION file, use it when
  # determining which packages to update
  if (file.exists(file.path(project, "DESCRIPTION"))) {
    
    result <- pak$local_install_dev_deps(
      root = project,
      lib  = library[[1L]],
      ask  = prompt
    )
    
    return(result)
  }
  
  # read description files for all installed packages
  # TODO: do we want to also update packages in other library paths,
  # or just packages installed in the project library?
  records <- renv_snapshot_libpaths(library[[1L]], project = project)
  remotes <- map_chr(records, renv_record_format_remote, versioned = FALSE, pak = TRUE)
  if (length(remotes) == 0L) {
    caution("- There are no packages to update.")
    return(invisible(NULL))
  }
  
  # update those packages
  pak$pkg_install(
    pkg = unname(remotes),
    lib = library[[1L]],
    upgrade = TRUE,
    ask = prompt
  )

}

renv_pak_install <- function(packages,
                             library,
                             type,
                             rebuild,
                             prompt,
                             project)
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

  # if we received a named list of remotes, use the names
  packages <- if (any(nzchar(names(packages))))
     names(packages)
  else
    as.character(packages)

  # if no packages were specified, treat this as a request to
  # install / update packages used in the project
  if (length(packages) == 0L)
    return(renv_pak_update(project, library, prompt))
  
  # pak doesn't support ':' as a sub-directory separator, so try to
  # repair that here
  # https://github.com/rstudio/renv/issues/2011
  pattern <- "(?<!:):([^/#@:]+)"
  packages <- gsub(pattern, "/\\1", packages, perl = TRUE)
  
  # build parameters
  packages <- map_chr(packages, function(package) {

    params <- c(
      if (identical(type, "source")) "source",
      if (identical(rebuild, TRUE) || package %in% rebuild) "reinstall"
    )

    if (length(params))
      paste(package, paste(params, collapse = "&"), sep = "?")
    else
      package

  })

  pak$pkg_install(
    pkg     = packages,
    lib     = library[[1L]],
    ask     = prompt,
    upgrade = TRUE
  )
}

renv_pak_restore <- function(lockfile,
                             packages = NULL,
                             exclude = NULL,
                             prompt = FALSE,
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

  # convert into specs compatible with pak, and install
  remotes <- map_chr(records, renv_record_format_remote, pak = TRUE)

  # TODO: We previously tried converting version-ed remotes into "plain" remotes
  # if the package version happened to be current, but then 'pak' would choose
  # not to install the package if a newer version was available. Hence, we need
  # to preserve the exact remote we wish to install here.

  # return early if there are zero remotes to restore
  if (length(remotes) == 0L) {
    return(invisible(TRUE))
  }

  # perform installation
  pak$pkg_install(
    pkg = remotes,
    ask = prompt
  )

  # return installed records
  records

}

