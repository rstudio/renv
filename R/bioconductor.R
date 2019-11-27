
renv_bioconductor_init <- function() {
  if (getRversion() >= "3.5.0")
    renv_bioconductor_init_biocmanager()
  else
    renv_bioconductor_init_biocinstaller()
}

renv_bioconductor_init_biocmanager <- function() {

  location <- find.package("BiocManager", quiet = TRUE)
  if (!empty(location))
    return(TRUE)

  install("BiocManager")
  TRUE

}

renv_bioconductor_init_biocinstaller <- function() {

  location <- find.package("BiocInstaller", quiet = TRUE)
  if (!empty(location))
    return(TRUE)

  url <- "https://bioconductor.org/biocLite.R"
  destfile <- tempfile("renv-bioclite-", fileext = ".R")
  on.exit(unlink(destfile), add = TRUE)
  download(url, destfile = destfile, quiet = TRUE)

  source(destfile)
  TRUE

}

renv_bioconductor_version <- function() {

  if (renv_package_installed("BiocManager")) {
    BiocManager <- asNamespace("BiocManager")
    format(BiocManager$version())
  } else if (renv_package_installed("BiocInstaller")) {
    BiocInstaller <- asNamespace("BiocInstaller")
    format(BiocInstaller$biocVersion())
  } else if (renv_package_installed("BiocVersion")) {
    format(packageVersion("BiocVersion")[1, 1:2])
  }

}

renv_bioconductor_repos <- function(version = NULL) {

  # allow bioconductor repos override
  repos <- getOption("renv.bioconductor.repos")
  if (!is.null(repos))
    return(repos)

  # read Bioconductor version (normally set during restore)
  version <- version %||% getOption("renv.bioconductor.version")

  # try both BiocManager, BiocInstaller to get Bioconductor repositories
  getters <- list(

    BiocManager = function() {
      BiocManager <- asNamespace("BiocManager")
      version <- version %||% BiocManager$version()
      BiocManager$repositories(version = version)
    },

    BiocInstaller = function() {
      BiocInstaller <- asNamespace("BiocInstaller")
      version <- version %||% BiocInstaller$biocVersion()
      BiocInstaller$biocinstallRepos(version = version)
    }

  )

  # prefer BiocInstaller for older versions of R
  if (getRversion() < "3.5.0")
    getters <- rev(getters)

  # now, try asking the packages for the active repositories
  for (getter in getters) {
    repos <- catch(getter())
    if (!inherits(repos, "error"))
      return(repos)
  }

  stopf("failed to determine Bioconductor repositories")

}
