
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

# determine whether a package's DESCRIPTION indicates it should be retrieved
# from Bioconductor -- that is, from the "auxiliary" Bioconductor repositories
# rather than from the user's regular (CRAN-like) repositories.
#
# this is deliberately a question about where the package was *obtained*, not
# about where it originated. a genuine Bioconductor package can also be served
# from a CRAN-like repository (e.g. a Posit Package Manager "R repository" that
# mixes CRAN and Bioconductor packages); when it is, renv should restore it from
# that repository, so we must answer FALSE.
#
# the result governs both directions of the same decision:
#   - on snapshot/restore, whether the package's Source is "Bioconductor"
#   - on install, whether to activate the Bioconductor repositories before
#     resolving its dependencies (see renv_retrieve_*, renv_graph_*)
# both want the same answer because both turn on the same fact: was this copy
# obtained from Bioconductor? if not, the repository it came from supplies its
# Bioconductor dependencies too, so the Bioconductor repositories must stay out
# of the way.
# https://github.com/rstudio/renv/issues/2128
renv_description_bioconductor <- function(dcf) {

  # 'biocViews' is necessary but not sufficient: some CRAN packages declare it
  # too, so on its own it cannot prove the package came from Bioconductor
  if (!nzchar(dcf[["biocViews"]] %||% ""))
    return(FALSE)

  # the 'Repository' field records where the package was obtained, and when
  # present it is decisive. the definitive Bioconductor stamp is 'Bioconductor
  # <version>': bioconductor.org uses it, and Posit Package Manager Bioconductor
  # repositories include 'Bioconductor' in the stamp too (possibly alongside a
  # custom repository name).
  repository <- dcf[["Repository"]] %||% ""
  if (grepl("Bioconductor", repository, ignore.case = TRUE))
    return(TRUE)

  # Bioconductor also ships binaries via r-universe, where the 'Repository'
  # stamp is a 'https://bioc-*.r-universe.dev' URL that carries no
  # 'Bioconductor' in it. accept that specific shape -- but nothing broader: a
  # bare 'bioc' could appear in an unrelated repository name, and a
  # non-Bioconductor r-universe is not Bioconductor, so both signals are
  # required.
  if (grepl("bioc", repository, ignore.case = TRUE) &&
      grepl("r-universe[.]dev", repository, ignore.case = TRUE))
    return(TRUE)

  # any other non-empty 'Repository' value (CRAN, RSPM, a custom repo name, a
  # non-Bioconductor r-universe, ...) means the package was obtained from there,
  # so it is not Bioconductor and should be restored from that repository. this
  # is what separates a genuine Bioconductor install from a Bioconductor package
  # re-served by a CRAN-like repository: Posit Package Manager stamps
  # 'Repository: RSPM' on the latter even though it still carries Bioconductor
  # git provenance.
  # https://github.com/rstudio/renv/issues/2128
  if (nzchar(repository))
    return(FALSE)

  # no 'Repository' stamp at all: nothing records where this copy was obtained,
  # so trust 'biocViews'. this is how bioconductor.org binaries arrive (they
  # carry only Bioconductor git provenance, e.g. a 'git_url' on the Bioconductor
  # git server), and it also covers very old or source-installed Bioconductor
  # packages that were never stamped with any provenance fields at all. this is
  # renv's long-standing behaviour, and it is only reached when 'Repository' is
  # absent, so it cannot misclassify an RSPM-served package -- that carries a
  # 'Repository' stamp and was rejected above. (a non-Bioconductor package that
  # declares 'biocViews' and is installed with no 'Repository' stamp -- e.g. via
  # 'R CMD INSTALL' of a source tarball -- would be treated as Bioconductor
  # here, but such installs are rare; the common ones, GitHub and local, declare
  # a 'RemoteType' and are classified before this function is consulted.)
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
