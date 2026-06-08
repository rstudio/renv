
renv_bioconductor_manager <- function() {
  if (getRversion() >= "3.5.0")
    "BiocManager"
  else
    "BiocInstaller"
}

# whether Bioconductor integration is enabled for this project; when disabled,
# renv will not infer a Bioconductor source, activate Bioconductor repositories,
# or write a Bioconductor entry into the lockfile
# https://github.com/rstudio/renv/issues/2128
renv_bioconductor_enabled <- function(project = NULL) {
  settings$bioconductor.enabled(project = project)
}

# determine whether a package's DESCRIPTION genuinely indicates a Bioconductor
# origin. historically renv treated the mere presence of a 'biocViews' field as
# proof, but some CRAN packages also declare 'biocViews', and Posit Package
# Manager can serve Bioconductor packages from a CRAN-like "R repository". what
# matters for restore is where the package was *obtained* (the 'Repository'
# field), not where it originally came from (its git provenance).
# https://github.com/rstudio/renv/issues/2128
renv_description_bioconductor <- function(dcf) {

  # packages from Bioconductor are normally tagged with a 'biocViews' entry;
  # without one, this cannot be a Bioconductor package
  if (!nzchar(dcf[["biocViews"]] %||% ""))
    return(FALSE)

  # the 'Repository' field records where the package was obtained, and so takes
  # precedence over git provenance. Bioconductor stamps 'Bioconductor <ver>'; a
  # CRAN-like repository (CRAN, a known mirror, or one of the active repos --
  # including PPM "R repositories" that serve Bioconductor in a CRAN-like
  # layout) means the package should be restored from that repository instead.
  repository <- dcf[["Repository"]] %||% ""
  if (nzchar(repository)) {

    # Bioconductor stamps the release into the 'Repository' field (e.g.
    # 'Bioconductor 3.18'); Posit Package Manager Bioconductor repositories
    # likewise include 'Bioconductor' in the stamp, possibly alongside a custom
    # repository name, so match it anywhere in the field
    if (grepl("Bioconductor", repository, ignore.case = TRUE))
      return(TRUE)

    # otherwise, a 'Repository' that names CRAN, a known CRAN mirror, or one of
    # the active repositories means the package came from there rather than from
    # Bioconductor.
    #
    # NOTE: this repository-recognition logic parallels the matching done in
    # renv_snapshot_description_source_custom(); the two are intentionally kept
    # separate because they disagree on the bare name "CRAN". here we *trust*
    # 'Repository: CRAN' as proof of CRAN origin, whereas _source_custom()
    # deliberately *distrusts* a "CRAN" RemoteReposName (older renv versions
    # mislabelled non-CRAN repositories as 'CRAN'; see #2104). if these ever
    # need to share an implementation, extract a helper parameterised on whether
    # the "CRAN" name should be honoured, rather than collapsing them outright.
    if (identical(repository, "CRAN"))
      return(FALSE)

    mirrors <- renv_cran_mirrors()
    if (any(renv_repos_matches(repository, mirrors)))
      return(FALSE)

    repos <- as.list(getOption("repos"))
    if (repository %in% names(repos) || any(renv_repos_matches(repository, repos)))
      return(FALSE)

  }

  # no informative 'Repository'; fall back to Bioconductor git provenance, which
  # Bioconductor-built tarballs carry pointing at the Bioconductor git server
  git_url <- dcf[["git_url"]] %||% ""
  if (grepl("bioconductor", git_url, ignore.case = TRUE))
    return(TRUE)

  # 'biocViews' present with no contradicting signal; preserve the historical
  # behaviour of trusting it (e.g. a sources install of a real Bioconductor
  # package that was never stamped with provenance fields)
  TRUE

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

# does Bioconductor support the version of R currently running?
#
# BiocManager maps each version of R to one or more Bioconductor releases, each
# with a status ('release', 'devel', 'out-of-date', or 'future'). when R is
# newer than any usable Bioconductor release -- for example, on R-devel before
# Bioconductor has caught up -- the only entry for that version of R is a
# 'future' release, which has no installable packages yet.
renv_bioconductor_supported <- function() {

  if (!renv_package_available("BiocManager"))
    return(FALSE)

  # read BiocManager's R-to-Bioconductor version map
  map <- catch({
    BiocManager <- renv_scope_biocmanager()
    BiocManager$.version_map()
  })

  ok <-
    !inherits(map, "error") &&
    is.data.frame(map) &&
    all(c("R", "BiocStatus") %in% names(map))

  if (!ok)
    return(FALSE)

  # require an entry for the running version of R whose Bioconductor release has
  # a usable status (anything other than 'future', which has no packages yet)
  rversion <- paste(getRversion()[1L, 1L:2L])
  matches <- as.character(map$R) == rversion & as.character(map$BiocStatus) != "future"
  any(matches)

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

  # get current repositories
  repos <- getOption("repos")

  # read Bioconductor repositories (prefer BiocInstaller for older R)
  biocrepos <- if (identical(renv_bioconductor_manager(), "BiocManager"))
    renv_bioconductor_repos_biocmanager(version)
  else
    renv_bioconductor_repos_biocinstaller(version)

  # overlay new repos on old repos (this helps preserve ordering)
  # https://github.com/rstudio/renv/issues/2128
  repos[names(biocrepos)] <- biocrepos
  repos

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
