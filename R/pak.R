
renv_pak_init <- function(library, type, rebuild, project) {

  if (requireNamespace("pak", quietly = TRUE))
    return(renv_namespace_load("pak"))

  install(
    packages = "pak",
    library  = library,
    type     = type,
    project  = project
  )

}

renv_pak_install <- function(packages, library) {

  pak <- renv_namespace_load("pak")
  pak$pkg_install(
    pkg     = unlist(packages, use.names = FALSE),
    lib     = library[[1L]],
    upgrade = TRUE
  )

}
