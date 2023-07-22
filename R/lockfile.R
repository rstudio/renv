
renv_lockfile_init <- function(project) {

  lockfile <- list()

  lockfile$R        <- renv_lockfile_init_r(project)
  lockfile$Python   <- renv_lockfile_init_python(project)
  lockfile$Packages <- list()

  class(lockfile) <- "renv_lockfile"
  lockfile

}

renv_lockfile_init_r_version <- function(project) {

  # NOTE: older versions of renv may have written out an empty array
  # for the R version in some cases, so we explicitly check that we
  # receive a length-one string here.
  version <- settings$r.version(project = project)
  if (!pstring(version))
    version <- getRversion()

  format(version)

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
  repos[repos == "@CRAN@"] <- getOption(
    "renv.repos.cran",
    "https://cloud.r-project.org"
  )

  # remove PPM bits from URL
  if (renv_ppm_enabled()) {
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

  fields <- list()

  fields$Version <- version
  fields$Type    <- type
  fields$Name    <- name

  fields

}

renv_lockfile_fini <- function(lockfile, project) {
  lockfile$Bioconductor <- renv_lockfile_fini_bioconductor(lockfile, project)
  lockfile
}

renv_lockfile_fini_bioconductor <- function(lockfile, project) {

  # check for explicit version in settings
  version <- settings$bioconductor.version(project = project)
  if (length(version))
    return(list(Version = version))

  # otherwise, check for a package which required Bioconductor
  records <- renv_lockfile_records(lockfile)
  if (empty(records))
    return(NULL)

  for (package in c("BiocManager", "BiocInstaller"))
    if (!is.null(records[[package]]))
      return(list(Version = renv_bioconductor_version(project = project)))

  sources <- extract_chr(records, "Source")
  if ("Bioconductor" %in% sources)
    return(list(Version = renv_bioconductor_version(project = project)))

  # nothing found; return NULL
  NULL

}

renv_lockfile_path <- function(project) {
  renv_paths_lockfile(project = project)
}

renv_lockfile_save <- function(lockfile, project) {
  file <- renv_lockfile_path(project)
  renv_lockfile_write(lockfile, file = file)
}

renv_lockfile_load <- function(project, strict = FALSE) {

  path <- renv_lockfile_path(project)
  if (file.exists(path))
    return(renv_lockfile_read(path))

  if (strict) {
    abort(c(
      "This project does not contain a lockfile.",
      i = "Have you called `snapshot()` yet?"
    ))
  }

  renv_lockfile_init(project = project)

}

renv_lockfile_sort <- function(lockfile) {

  # extract R records (nothing to do if empty)
  records <- renv_lockfile_records(lockfile)
  if (empty(records))
    return(lockfile)

  # sort the records
  sorted <- records[csort(names(records))]
  renv_lockfile_records(lockfile) <- sorted

  # sort top-level fields
  fields <- unique(c("R", "Bioconductor", "Python", "Packages", names(lockfile)))
  lockfile <- lockfile[intersect(fields, names(lockfile))]

  # return post-sort
  lockfile

}

renv_lockfile_create <- function(project,
                                 type = NULL,
                                 libpaths = NULL,
                                 packages = NULL,
                                 exclude = NULL,
                                 prompt = NULL,
                                 force = NULL)
{
  libpaths <- libpaths %||% renv_libpaths_all()
  type <- type %||% settings$snapshot.type(project = project)

  # use a restart, so we can allow the user to install packages before snapshot
  lockfile <- withRestarts(
    renv_lockfile_create_impl(project, type, libpaths, packages, exclude, prompt, force),
    renv_recompute_records = function() {
      renv_dynamic_reset()
      renv_lockfile_create_impl(project, type, libpaths, packages, exclude, prompt, force)
    }
  )
}

renv_lockfile_create_impl <- function(project, type, libpaths, packages, exclude, prompt, force) {

  lockfile <- renv_lockfile_init(project)

  # compute the project's top-level package dependencies
  packages <- packages %||% renv_snapshot_dependencies(
    project = project,
    type = type,
    dev = FALSE
  )

  # expand the recursive dependencies of these packages
  records <- renv_snapshot_packages(
    packages = setdiff(packages, exclude),
    libpaths = libpaths,
    project  = project
  )

  # check for missing packages
  ignored <- c(renv_project_ignored_packages(project), renv_packages_base(), exclude, "renv")
  missing <- setdiff(packages, c(names(records), ignored))

  # cancel automatic snapshots if we have missing packages
  if (length(missing) && the$auto_snapshot_running)
    invokeRestart("cancel")

  # give user a chance to handle missing packages, if any
  #
  # we only run this in top-level calls to snapshot() since renv will internally
  # use snapshot() to create lockfiles, and missing packages are understood /
  # tolerated there. this code mostly exists so interactive usages of snapshot()
  # can recover and install missing packages
  if (identical(topfun(), snapshot))
    renv_snapshot_report_missing(missing, type)

  records <- renv_snapshot_fixup(records)
  renv_lockfile_records(lockfile) <- records

  lockfile <- renv_lockfile_fini(lockfile, project)

  keys <- unique(c("R", "Bioconductor", names(lockfile)))
  lockfile <- lockfile[intersect(keys, names(lockfile))]

  class(lockfile) <- "renv_lockfile"
  lockfile

}

renv_lockfile_modify <- function(lockfile, records) {

  enumerate(records, function(package, record) {
    renv_lockfile_records(lockfile)[[package]] <<- record
  })

  lockfile

}

renv_lockfile_compact <- function(lockfile) {

  records <- renv_lockfile_records(lockfile)
  remotes <- map_chr(records, renv_record_format_remote)

  remotes <- csort(remotes)

  formatted <- sprintf("  \"%s\"", remotes)
  joined <- paste(formatted, collapse = ",\n")

  all <- c("renv::use(", joined, ")")
  paste(all, collapse = "\n")

}

renv_lockfile_records <- function(lockfile) {
  as.list(lockfile$Packages %||% lockfile)
}

`renv_lockfile_records<-` <- function(x, value) {
  x$Packages <- filter(value, zlength)
  invisible(x)
}

# for compatibility with older versions of RStudio
renv_records <- renv_lockfile_records
