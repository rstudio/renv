
renv_bioconductor_manager <- function() {
  if (getRversion() >= "3.5.0")
    "BiocManager"
  else
    "BiocInstaller"
}

renv_bioconductor_versions <- function() {

  # map versions of Bioconductor to the versions of R they can be used with
  list(
    "3.9"  = "3.6",
    "3.10" = "3.6",
    "3.11" = "4.0",
    "3.12" = "4.0",
    "3.13" = "4.1",
    "3.14" = "4.1",
    "3.15" = "4.2",
    "3.16" = "4.2",
    "3.17" = "4.3",
    "3.18" = "4.3",
    "3.19" = "4.4",
    "3.20" = "4.4",
    "3.21" = "4.5",  # speculative
    "3.22" = "4.5"   # speculative
  )

}

renv_bioconductor_validate <- function(version, prompt = interactive()) {

  # check for the requested Bioconductor version in our internal version map;
  # if it doesn't exist, then just assume compatibility
  #
  # we previously used BiocManager for this, but because it makes web requests,
  # this can be prohibitively slow for certain users
  #
  # https://github.com/rstudio/renv/issues/2091
  biocversions <- renv_bioconductor_versions()
  rversion <- biocversions[[version]]
  if (is.null(rversion))
    return(TRUE)

  # check that the version of R in use matches what Bioconductor requires
  ok <- renv_version_eq(rversion, getRversion(), n = 2L)
  if (ok)
    return(TRUE)

  fmt <- lines(
    "You are using Bioconductor %1$s, which is not compatible with R %2$s.",
    "Use 'renv::init(bioconductor = TRUE)' to re-initialize this project with the appropriate Bioconductor release.",
    if (renv_package_installed("BiocVersion"))
      "Please uninstall the 'BiocVersion' package first, with `remove.packages(\"BiocVersion\")`."
  )

  caution(fmt, version, getRversion())

  if (prompt) {
    writef("")
    response <- ask("Would you still like to use this version of Bioconductor?")
    cancel_if(!response)
  }

  TRUE

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
  install("BiocManager", library = library, prompt = FALSE)

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
