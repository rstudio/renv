
renv_migrate_packrat <- function(project = NULL) {
  project <- project %||% renv_project()

  if (!requireNamespace("packrat", quietly = TRUE))
    stopf("migration requires the 'packrat' package to be installed")

  renv_migrate_packrat_lockfile(project)
  renv_migrate_packrat_library(project)
  renv_migrate_packrat_options(project)
  renv_migrate_packrat_cache(project)

  fmt <- "* The Packrat infrastructure for project '%s' has been migrated to renv."
  vwritef(fmt, aliased_path(project))
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
  lockfile <- renv_lockfile_init(project)

  # update fields
  lockfile$R$Version <- header$RVersion
  lockfile$R$Repositories <- as.character(repos)
  lockfile$R$Packages <- records

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

  # cache each installed package
  vprintf("* Migrating library from Packrat to renv ... ")
  ensure_parent_directory(targets)
  copy <- renv_progress(renv_file_copy, length(targets))
  enumerate(targets, copy)
  vwritef("Done!")

  TRUE

}

renv_migrate_packrat_options <- function(project) {

  packrat <- asNamespace("packrat")
  opts <- packrat$get_opts(project = project)

  settings$ignored.packages(opts$ignored.packages)
}

## TODO: this may not be safe since the Packrat cache
## is not properly versioned by default; but perhaps
## we can infer the appropriate R version from the
## DESCRIPTION
#
renv_migrate_packrat_cache <- function(project) {

  # find packages in the packrat cache
  packrat <- asNamespace("packrat")
  cache <- packrat$cacheLibDir()
  packages <- list.files(cache, full.names = TRUE)
  hashes <- list.files(packages, full.names = TRUE)
  sources <- list.files(hashes, full.names = TRUE)

  # read DESCRIPTIONs for each package
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
  vprintf("* Migrating cached packages from Packrat to renv ... ")
  ensure_parent_directory(targets)
  copy <- renv_progress(renv_file_copy, length(targets))
  enumerate(targets, copy)
  vwritef("Done!")

  TRUE

}
