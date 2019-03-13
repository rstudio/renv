
renv_bioconductor_repos <- function() {

  # if the repositories have already been set, use them
  repos <- getOption("bioconductor.repos")
  if (!is.null(repos))
    return(repos)

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
