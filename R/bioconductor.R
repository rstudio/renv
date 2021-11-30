
renv_bioconductor_init <- function() {
  if (getRversion() >= "3.5.0")
    renv_bioconductor_init_biocmanager()
  else
    renv_bioconductor_init_biocinstaller()
}

renv_bioconductor_init_biocmanager <- function() {

  if (renv_package_available("BiocManager"))
    return(TRUE)

  install("BiocManager")
  TRUE

}

renv_bioconductor_init_biocinstaller <- function() {

  if (renv_package_available("BiocInstaller"))
    return(TRUE)

  url <- "https://bioconductor.org/biocLite.R"
  destfile <- tempfile("renv-bioclite-", fileext = ".R")
  on.exit(unlink(destfile), add = TRUE)
  download(url, destfile = destfile, quiet = TRUE)

  source(destfile)
  TRUE

}

renv_bioconductor_version <- function(project) {

  # check and see if we have an override via option
  version <- getOption("renv.bioconductor.version")
  if (!is.null(version))
    return(version)

  # check and see if the project has been configured to use a specific
  # Bioconductor release
  version <- settings$bioconductor.version(project = project)
  if (length(version))
    return(version)

  # otherwise, infer the Bioconductor version from installed packages
  case(

    renv_package_available("BiocVersion") ~ {
      format(packageVersion("BiocVersion")[1, 1:2])
    },

    renv_package_available("BiocManager") ~ {
      BiocManager <- renv_namespace_load("BiocManager")
      format(BiocManager$version())
    },

    renv_package_available("BiocVersion") ~ {
      BiocInstaller <- renv_namespace_load("BiocInstaller")
      format(BiocInstaller$biocVersion())
    }

  )

}

renv_bioconductor_repos <- function(project, version = NULL) {

  # allow bioconductor repos override
  repos <- getOption("renv.bioconductor.repos")
  if (!is.null(repos))
    return(repos)

  # read Bioconductor version (normally set during restore)
  version <- version %||% renv_bioconductor_version(project = project)

  # read Bioconductor repositories (prefer BiocInstaller for older R)
  if (getRversion() < "3.5.0")
    renv_bioconductor_repos_biocinstaller(version)
  else
    renv_bioconductor_repos_biocmanager(version)

}

renv_bioconductor_repos_biocmanager <- function(version) {
  renv_scope_options(BiocManager.check_repositories = FALSE)
  BiocManager <- asNamespace("BiocManager")
  version <- version %||% BiocManager$version()
  BiocManager$repositories(version = version)
}

renv_bioconductor_repos_biocinstaller <- function(version) {
  BiocInstaller <- asNamespace("BiocInstaller")
  version <- version %||% BiocInstaller$biocVersion()
  BiocInstaller$biocinstallRepos(version = version)
}
