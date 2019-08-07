
#' Migrate a Project from Packrat to renv
#'
#' Migrate a project's infrastructure from Packrat to `renv`.
#'
#' The following actions are taken:
#'
#' - The Packrat lockfile is migrated to `renv.lock`;
#' - Packages installed in the Packrat library are migrated to the `renv` library;
#' - Relevant Packrat options (e.g. `ignored.packages`) are copied;
#' - Packages in the Packrat cache are imported into the `renv` cache;
#' - The project `.Rprofile` is updated to use `renv`.
#'
#' @inheritParams renv-params
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # migrate Packrat project infrastructure to renv
#' renv::migrate()
#'
#' }
migrate <- function(project = NULL) {
  project <- project %||% renv_project()
  renv_scope_error_handler()

  project <- normalizePath(project, winslash = "/", mustWork = TRUE)
  if (file.exists(file.path(project, "packrat/packrat.lock")))
    renv_migrate_packrat(project)
}

renv_migrate_packrat <- function(project = NULL) {
  project <- project %||% renv_project()

  if (!requireNamespace("packrat", quietly = TRUE))
    stopf("migration requires the 'packrat' package to be installed")

  renv_migrate_packrat_lockfile(project)
  renv_migrate_packrat_library(project)
  renv_migrate_packrat_options(project)
  renv_migrate_packrat_cache(project)
  renv_migrate_packrat_infrastructure(project)

  renv_bootstrap_impl()

  fmt <- "* Project '%s' has been migrated from Packrat to renv."
  vwritef(fmt, aliased_path(project))

  vwritef("* Consider deleting the project 'packrat' folder if it is no longer needed.")
  invisible(TRUE)
}

renv_migrate_packrat_lockfile <- function(project) {

  plock <- file.path(project, "packrat/packrat.lock")
  if (!file.exists(plock))
    return(FALSE)

  # read the lockfile
  contents <- read(plock)
  splat <- strsplit(contents, "\n{2,}")[[1]]
  dcf <- lapply(splat, function(section) {
    conn <- textConnection(section)
    on.exit(close(conn), add = TRUE)
    renv_dcf_read(conn)
  })

  # split into header + package fields
  header <- dcf[[1]]
  records <- dcf[-1L]

  # parse the repositories
  repos <- getOption("repos")
  if (!is.null(header$Repos)) {
    parts <- strsplit(header$Repos, "\\s*,\\s*")[[1]]
    repos <- renv_read_properties(text = parts, delimiter = "=")
  }

  # fix-up some record fields for renv
  records <- lapply(records, function(record) {
    record$Hash <- NULL
    as.list(record)
  })

  # pull out names for records
  names(records) <- extract_chr(records, "Package")

  # generate a blank lockfile
  lockfile <- list()
  lockfile$renv <- list(Version = renv_package_version("renv"))
  lockfile$R    <- renv_lockfile_init_r(project)

  # update fields
  lockfile$R$Version <- header$RVersion
  lockfile$R$Repositories <- as.list(repos)
  lockfile$Packages <- records

  # write the lockfile
  lockpath <- file.path(project, "renv.lock")
  renv_lockfile_write(lockfile, file = lockpath)

}

renv_migrate_packrat_library <- function(project) {

  packrat <- asNamespace("packrat")
  sources <- list.files(packrat$libDir(project = project), full.names = TRUE)
  targets <- renv_paths_library(basename(sources), project = project)

  names(targets) <- sources
  targets <- targets[!file.exists(targets)]
  if (empty(targets)) {
    vwritef("* The renv library is already synchronized with the Packrat library.")
    return(TRUE)
  }

  # copy packages from Packrat to renv private library
  vprintf("* Migrating library from Packrat to renv ... ")
  ensure_parent_directory(targets)
  copy <- renv_progress(renv_file_copy, length(targets))
  enumerate(targets, copy)
  vwritef("Done!")

  # move packages into the cache
  if (settings$use.cache(project = project)) {
    vprintf("* Moving packages into the renv cache ... ")
    records <- lapply(targets, renv_description_read)
    sync <- renv_progress(renv_cache_synchronize, length(targets))
    lapply(records, sync, linkable = TRUE)
    vwritef("Done!")
  }

  TRUE

}

renv_migrate_packrat_options <- function(project) {

  packrat <- asNamespace("packrat")
  opts <- packrat$get_opts(project = project)

  settings$ignored.packages(opts$ignored.packages, project = project)

}

renv_migrate_packrat_cache <- function(project) {

  # find packages in the packrat cache
  packrat <- asNamespace("packrat")
  cache <- packrat$cacheLibDir()
  packages <- list.files(cache, full.names = TRUE)
  hashes <- list.files(packages, full.names = TRUE)
  sources <- list.files(hashes, full.names = TRUE)

  # read DESCRIPTIONs for each package (update the Hash
  # as Packrat + renv hashes are not compatible)
  records <- lapply(sources, function(source) {
    record <- renv_description_read(source)
    record$Hash <- renv_hash_description(source)
    record
  })

  # construct cache target paths
  targets <- map_chr(records, renv_cache_package_path)
  names(targets) <- sources
  targets <- targets[!file.exists(targets)]
  if (empty(targets)) {
    vwritef("* The renv cache is already synchronized with the Packrat cache.")
    return(TRUE)
  }

  # cache each installed package
  if (settings$use.cache(project = project)) {
    vprintf("* Migrating Packrat cache to renv cache ... ")
    ensure_parent_directory(targets)
    copy <- renv_progress(renv_file_copy, length(targets))
    enumerate(targets, copy)
    vwritef("Done!")
  }

  TRUE

}

renv_migrate_packrat_infrastructure <- function(project) {
  unlink(file.path(project, ".Rprofile"))
  renv_infrastructure_write(project)
  vwritef("* renv support infrastructure has been written.")
  TRUE
}
