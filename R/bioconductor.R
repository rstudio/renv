
renv_bioconductor_init <- function() {
  if (getRversion() >= "3.5.0") {
    renv_bioconductor_init_biocmanager()
  } else {
    renv_bioconductor_init_biocinstaller()
  }
}

renv_bioconductor_init_biocmanager <- function() {

  location <- find.package("BiocManager", quiet = TRUE)
  if (!empty(location))
    return(TRUE)

  utils::install.packages("BiocManager", quiet = TRUE)
  TRUE

}

renv_bioconductor_init_biocinstaller <- function() {

  location <- find.package("BiocInstaller", quiet = TRUE)
  if (!empty(location))
    return(TRUE)

  source("https://bioconductor.org/biocLite.R")
  TRUE

}

renv_bioconductor_repos <- function() {

  # try both BiocManager, BiocInstaller to get Bioc repositories
  getters <- list(

    BiocManager = function() {
      BiocManager <- asNamespace("BiocManager")
      BiocManager$repositories()
    },

    BiocInstaller = function() {
      BiocInstaller <- asNamespace("BiocInstaller")
      BiocInstaller$biocinstallRepos()
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
