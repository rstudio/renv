
renv_lockfile_init <- function(project) {

  lockfile <- list()

  lockfile$R            <- renv_lockfile_init_r(project)
  lockfile$Python       <- renv_lockfile_init_python(project)

  class(lockfile) <- "renv_lockfile"
  lockfile

}

renv_lockfile_init_r_version <- function(project) {
  format(getRversion())
}

renv_lockfile_init_r_repos <- function(project) {

  repos <- getOption("repos")

  # save names
  nms <- names(repos)

  # force as character
  repos <- as.character(repos)

  # clear RStudio attribute
  attr(repos, "RStudio") <- NULL

  # set a default URL
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"

  # remove RSPM bits from URL
  if (config$rspm.enabled()) {
    pattern <- "/__[^_]+__/[^/]+/"
    repos <- sub(pattern, "/", repos)
  }

  # force as list
  repos <- as.list(repos)

  # ensure names
  names(repos) <- nms

  repos

}

renv_lockfile_init_r <- function(project) {
  version <- renv_lockfile_init_r_version(project)
  repos   <- renv_lockfile_init_r_repos(project)
  list(Version = version, Repositories = repos)
}

renv_lockfile_init_python <- function(project) {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python))
    return(NULL)

  if (!file.exists(python))
    return(NULL)

  info <- renv_python_info(python)
  if (is.null(info))
    return(NULL)

  version <- renv_python_version(python)
  type <- info$type
  root <- info$root
  name <- renv_python_envname(project, root, type)

  list(Version = version, Type = type, Name = name)
}

renv_lockfile_fini <- function(lockfile) {
  lockfile$Bioconductor <- renv_lockfile_fini_bioconductor(lockfile)
  lockfile
}

renv_lockfile_fini_bioconductor <- function(lockfile) {

  records <- renv_records(lockfile)
  if (empty(records))
    return(NULL)

  for (package in c("BiocManager", "BiocInstaller"))
    if (!is.null(records[[package]]))
      return(list(Version = renv_bioconductor_version()))

  sources <- extract_chr(records, "Source")
  if ("Bioconductor" %in% sources)
    return(list(Version = renv_bioconductor_version()))

}

renv_lockfile_path <- function(project) {
  file.path(project, "renv.lock")
}

renv_lockfile_save <- function(lockfile, project) {
  renv_lockfile_write(lockfile, file = renv_lockfile_path(project))
}

renv_lockfile_load <- function(project) {

  path <- renv_lockfile_path(project)
  if (file.exists(path))
    return(renv_lockfile_read(path))

  renv_lockfile_init(project = project)

}

renv_lockfile_sort <- function(lockfile) {

  # ensure C locale for consistent sorting
  renv_scope_locale("LC_COLLATE", "C")

  # extract R records (nothing to do if empty)
  records <- renv_records(lockfile)
  if (empty(records))
    return(lockfile)

  # sort the records
  sorted <- records[sort(names(records))]
  renv_records(lockfile) <- sorted

  # return post-sort
  lockfile

}

renv_lockfile_create <- function(project, library, type) {

  lockfile <- renv_lockfile_init(project)

  renv_records(lockfile) <-
    renv_snapshot_r_packages(library = library, project = project) %>%
    renv_snapshot_filter(project = project, type = type) %>%
    renv_snapshot_fixup()

  lockfile <- renv_lockfile_fini(lockfile)

  keys <- unique(c("R", "Bioconductor", names(lockfile)))
  lockfile <- lockfile[intersect(keys, names(lockfile))]

  class(lockfile) <- "renv_lockfile"
  lockfile

}

renv_lockfile_modify <- function(lockfile, records) {

  enumerate(records, function(package, record) {
    renv_records(lockfile)[[package]] <<- record
  })

  lockfile

}
