
renv_pak_init <- function() {

  if (requireNamespace("pak", quietly = TRUE))
    return(renv_namespace_load("pak"))

  install.packages("pak")
  renv_namespace_load("pak")

}

renv_pak_install <- function(packages, library) {

  pak <- renv_namespace_load("pak")
  pak$pkg_install(
    pkg     = unlist(packages, use.names = FALSE),
    lib     = library[[1L]],
    upgrade = TRUE
  )

}
