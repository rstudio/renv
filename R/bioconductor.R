
renv_bioconductor_manager <- function() {
  if (getRversion() >= "3.5.0")
    "BiocManager"
  else
    "BiocInstaller"
}

renv_bioconductor_init <- function(library = NULL) {
  renv_scope_options(renv.verbose = FALSE)

  if (identical(renv_bioconductor_manager(), "BiocManager"))
    renv_bioconductor_init_biocmanager(library)
  else
    renv_bioconductor_init_biocinstaller(library)
}

renv_bioconductor_init_biocmanager <- function(library = NULL) {

  library <- library %||% renv_libpaths_active()
  if (renv_package_installed("BiocManager", lib.loc = library))
    return(TRUE)

  ensure_directory(library)
  install("BiocManager", library = library)
  TRUE

}

renv_bioconductor_init_biocinstaller <- function(library = NULL) {

  library <- library %||% renv_libpaths_active()
  if (renv_package_installed("BiocInstaller", lib.loc = library))
    return(TRUE)

  url <- "https://bioconductor.org/biocLite.R"
  destfile <- renv_scope_tempfile("renv-bioclite-", fileext = ".R")
  download(url, destfile = destfile, quiet = TRUE)

  ensure_directory(library)
  renv_scope_libpaths(library)
  source(destfile)
  TRUE

}

renv_bioconductor_version <- function(project, refresh = FALSE) {

  # check and see if we have an override via option
  version <- getOption("renv.bioconductor.version")
  if (!is.null(version))
    return(version)

  # check and see if the project has been configured to use a specific
  # Bioconductor release
  if (!refresh) {
    version <- settings$bioconductor.version(project = project)
    if (length(version))
      return(version)
  }

  # if BiocVersion is installed, use it
  if (renv_package_available("BiocVersion"))
    return(format(packageVersion("BiocVersion")[1, 1:2]))

  # make sure the required bioc package is available
  renv_bioconductor_init()

  # otherwise, infer the Bioconductor version from installed packages
  case(

    renv_package_available("BiocManager") ~ {
      BiocManager <- renv_scope_biocmanager()
      format(BiocManager$version())
    },

    renv_package_available("BiocVersion") ~ {
      BiocInstaller <- renv_namespace_load("BiocInstaller")
      format(BiocInstaller$biocVersion())
    }

  )

}

# Returns the union of the inferred Bioconductor repositories, together with the
# current value of the 'repos' R option. The Bioconductor repositories are
# placed first in the repository list.
renv_bioconductor_repos <- function(project = NULL, version = NULL) {

  # allow bioconductor repos override
  repos <- getOption("renv.bioconductor.repos")
  if (!is.null(repos))
    return(repos)

  # make sure the required bioc package is available
  renv_bioconductor_init()

  # read Bioconductor version (normally set during restore)
  version <- version %||% renv_bioconductor_version(project = project)

  # read Bioconductor repositories (prefer BiocInstaller for older R)
  if (identical(renv_bioconductor_manager(), "BiocManager"))
    renv_bioconductor_repos_biocmanager(version)
  else
    renv_bioconductor_repos_biocinstaller(version)

}

renv_bioconductor_repos_biocmanager <- function(version) {

  BiocManager <- renv_scope_biocmanager()
  version <- version %||% BiocManager$version()

  tryCatch(
    BiocManager$.repositories(site_repository = character(), version = version),
    error = function(e) {
      BiocManager$repositories(version = version)
    }
  )

}

renv_bioconductor_repos_biocinstaller <- function(version) {
  BiocInstaller <- asNamespace("BiocInstaller")
  version <- version %||% BiocInstaller$biocVersion()
  BiocInstaller$biocinstallRepos(version = version)
}

renv_bioconductor_required <- function(records) {

  for (record in records)
    if (identical(record$Source, "Bioconductor"))
      return(TRUE)

  FALSE

}
