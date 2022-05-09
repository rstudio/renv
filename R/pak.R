
renv_pak_init <- function(library, type, rebuild, project) {

  if (requireNamespace("pak", quietly = TRUE))
    return(renv_namespace_load("pak"))

  # prefer using prebuilt binaries
  fmt <- "https://r-lib.github.io/p/pak/stable/%s/%s/%s"
  repos <- sprintf(fmt, .Platform$pkgType, version$os, version$arch)
  utils::install.packages("pak", repos = repos)

}

renv_pak_install <- function(packages, library) {

  pak <- renv_namespace_load("pak")
  pak$pkg_install(
    pkg     = packages,
    lib     = library[[1L]],
    upgrade = TRUE
  )

}
