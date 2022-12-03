
renv_pak_init <- function() {

  if (requireNamespace("pak", quietly = TRUE))
    return(renv_namespace_load("pak"))

  # prefer using prebuilt binaries
  fmt <- "https://r-lib.github.io/p/pak/stable/%s/%s/%s"
  repos <- sprintf(fmt, .Platform$pkgType, version$os, version$arch)
  utils::install.packages("pak", repos = repos)

}

renv_pak_install <- function(packages, library, project) {

  pak <- renv_namespace_load("pak")
  lib <- library[[1L]]

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
