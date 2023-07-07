
# controls whether hashes are computed when computing a snapshot
# can be scoped to FALSE when hashing is not necessary
the$snapshot_hash <- TRUE

#' Record current state of the project library in the lockfile
#'
#' @description
#' Call `renv::snapshot()` to update a [lockfile] with the current state of
#' dependencies in the project library. The lockfile can be used to later
#' [restore] these dependencies as required.
#'
#' It's also possible to call `renv::snapshot()` with a non-renv project,
#' in which case it will record the current state of dependencies in the
#' current library paths. This makes it possible to [restore] the current packages,
#' providing lightweight portability and reproducibility without isolation.
#'
#' If you want to automatically snapshot after each change, you can
#' set `config$config$auto.snapshot(TRUE)`, see `?config` for more details.
#'
#' # Snapshot types
#'
#' Depending on how you prefer to manage dependencies, you might prefer
#' selecting a different snapshot mode. The modes available are as follows:
#'
#' \describe{
#'
#' \item{`"implicit"`}{
#' (The default) Capture only packages which appear to be used in your project,
#' as determined by `renv::dependencies()`. This ensures that only the packages
#' actually required by your project will enter the lockfile; the downside
#' if it might be slow if your project contains a large number of files.
#' If speed becomes an issue, you might consider using `.renvignore` files to
#' limit which files renv uses for dependency discovery, or switching to
#' explicit mode, as described next.
#' }
#'
#' \item{`"explicit"`}{
#' Only capture packages which are explicitly listed in the project
#' `DESCRIPTION` file. This workflow is recommended for users who wish to
#' manage their project's \R package dependencies directly.
#' }
#'
#' \item{`"all"`}{
#' Capture all packages within the active \R libraries in the lockfile.
#' This is the quickest and simplest method, but may lead to undesired
#' packages (e.g. development dependencies) entering the lockfile.
#' }
#'
#' \item{`"custom"`}{
#' Like `"implicit"`, but use a custom user-defined filter instead. The filter
#' should be specified by the \R option `renv.snapshot.filter`, and should
#' either be a character vector naming a function (e.g. `"package::method"`),
#' or be a function itself. The function should only accept one argument (the
#' project directory), and should return a vector of package names to include
#' in the lockfile.
#' }
#'
#' }
#'
#' You can change the snapshot type for the current project with [settings()].
#' For example, the following code will switch to using `"explicit"` snapshots:
#'
#' ```
#' renv::settings$snapshot.type("explicit")
#' ```
#'
#' When the `packages` argument is set, `type` is ignored, and instead only the
#' requested set of packages, and their recursive dependencies, will be written
#' to the lockfile.
#'
#' @inherit renv-params
#'
#' @param library The \R libraries to snapshot. When `NULL`, the active \R
#'   libraries (as reported by `.libPaths()`) are used.
#'
#' @param lockfile The location where the generated lockfile should be written.
#'   By default, the lockfile is written to a file called `renv.lock` in the
#'   project directory. When `NULL`, the lockfile (as an \R object) is returned
#'   directly instead.
#'
#' @param type The type of snapshot to perform:
#'   * `"implict"`, (the default), uses all packages captured by [dependencies()].
#'   * `"explicit"` uses packages recorded in `DESCRIPTION`.
#'   * `"all"` uses all packages in the project library.
#'   * `"custom` uses a custom filter.
#'
#'   See **Snapshot type** below for more details.
#'
#' @param repos The \R repositories to be recorded in the lockfile. Defaults
#'   to the currently active package repositories, as retrieved by
#'   `getOption("repos")`.
#'
#' @param packages A vector of packages to be included in the lockfile. When
#'   `NULL` (the default), all packages relevant for the type of snapshot being
#'   performed will be included. When set, the `type` argument is ignored.
#'   Recursive dependencies of the specified packages will be added to the
#'   lockfile as well.
#'
#' @param exclude A vector of packages to be explicitly excluded from the lockfile.
#'   Note that transitive package dependencies will always be included, to avoid
#'   potentially creating an incomplete / non-functional lockfile.
#'
#' @param update Boolean; if the lockfile already exists, then attempt to update
#'   that lockfile without removing any prior package records.
#'
#' @param force Boolean; force generation of a lockfile even when pre-flight
#'   validation checks have failed?
#'
#' @param reprex Boolean; generate output appropriate for embedding the lockfile
#'   as part of a [reprex](https://www.tidyverse.org/help/#reprex)?
#'
#' @return The generated lockfile, as an \R object (invisibly). Note that
#'   this function is normally called for its side effects.
#'
#' @family reproducibility
#'
#' @export
#'
#' @example examples/examples-init.R
snapshot <- function(project  = NULL,
                     ...,
                     library  = NULL,
                     lockfile = paths$lockfile(project = project),
                     type     = settings$snapshot.type(project = project),
                     repos    = getOption("repos"),
                     packages = NULL,
                     exclude  = NULL,
                     prompt   = interactive(),
                     update   = FALSE,
                     force    = FALSE,
                     reprex   = FALSE)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_snapshot_auto_suppress_next()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  repos <- renv_repos_validate(repos)
  renv_scope_options(repos = repos)

  if (!is.null(lockfile))
    renv_activate_prompt("snapshot", library, prompt, project)

  libpaths <- renv_path_normalize(library %||% renv_libpaths_all())
  if (config$snapshot.validate())
    renv_snapshot_preflight(project, libpaths)

  # when packages is set, we treat this as an 'all' type snapshot, but
  # with explicit package filters turned on
  if (!is.null(packages)) {

    if (!missing(type)) {
      fmt <- "packages argument is set; type argument %s will be ignored"
      warningf(fmt, stringify(type))
    }

    type <- "packages"

  }

  alt <- new <- renv_lockfile_create(
    project  = project,
    type     = type,
    libpaths = libpaths,
    packages = packages,
    exclude  = exclude,
    prompt   = prompt,
    force    = force
  )

  if (is.null(lockfile))
    return(new)

  # if running as part of 'reprex', then render output inline
  if (reprex)
    return(renv_snapshot_reprex(new))

  # check for missing dependencies and warn if any are discovered
  # (note: use 'new' rather than 'alt' here as we don't want to attempt
  # validation on uninstalled packages)
  valid <- renv_snapshot_validate(project, new, libpaths)
  renv_snapshot_validate_report(valid, prompt, force)

  # get prior lockfile state
  old <- list()
  if (file.exists(lockfile)) {

    # read a pre-existing lockfile (if any)
    old <- renv_lockfile_read(lockfile)

    # preserve records from alternate OSes in lockfile
    alt <- renv_snapshot_preserve(old, new)

    # check if there are any changes in the lockfile
    diff <- renv_lockfile_diff(old, alt)
    if (empty(diff)) {
      writef("- The lockfile is already up to date.")
      return(renv_snapshot_successful(alt, prompt, project))
    }

  }

  # update new reference
  new <- alt

  # if we're only updating the lockfile, then merge any missing records
  # from 'old' back into 'new'
  if (update)
    for (package in names(old$Packages))
      new$Packages[[package]] <- new$Packages[[package]] %||% old$Packages[[package]]

  # report actions to the user
  actions <- renv_lockfile_diff_packages(old, new)
  if (prompt || renv_verbose())
    renv_snapshot_report_actions(actions, old, new)

  # request user confirmation
  cancel_if(length(actions) && file.exists(lockfile) && prompt && !proceed())

  # write it out
  ensure_parent_directory(lockfile)
  renv_lockfile_write(new, file = lockfile)

  # ensure the lockfile is .Rbuildignore-d
  renv_infrastructure_write_rbuildignore(project)

  # ensure the activate script is up-to-date
  renv_infrastructure_write_activate(project, create = FALSE)

  # return new records
  renv_snapshot_successful(new, prompt, project)
}

renv_snapshot_preserve <- function(old, new) {
  records <- filter(old$Packages, renv_snapshot_preserve_impl)
  if (length(records))
    new$Packages[names(records)] <- records
  new
}

renv_snapshot_preserve_impl <- function(record) {

  ostype <- tolower(record[["OS_type"]] %||% "")
  if (!nzchar(ostype))
    return(FALSE)

  altos <- if (renv_platform_unix()) "windows" else "unix"
  identical(ostype, altos)

}

renv_snapshot_preflight <- function(project, libpaths) {
  lapply(libpaths, renv_snapshot_preflight_impl, project = project)
  if (interactive())
    renv_snapshot_preflight_check_sources(project, libpaths[[1L]])
}

renv_snapshot_preflight_impl <- function(project, library) {
  renv_snapshot_preflight_library_exists(project, library)
}

renv_snapshot_preflight_library_exists <- function(project, library) {

  # check that we have a directory
  type <- renv_file_type(library, symlinks = FALSE)
  if (type == "directory")
    return(TRUE)

  # if the file exists but isn't a directory, fail
  if (nzchar(type)) {
    fmt <- "library '%s' exists but is not a directory"
    stopf(fmt, renv_path_aliased(library))
  }

  # the directory doesn't exist; perhaps the user hasn't called init
  if (identical(library, renv_paths_library(project = project))) {
    fmt <- "project '%s' has no private library -- have you called `renv::init()`?"
    stopf(fmt, renv_path_aliased(project))
  }

  # user tried to snapshot arbitrary but missing path
  fmt <- "library '%s' does not exist; cannot proceed"
  stopf(fmt, renv_path_aliased(library))

}

renv_snapshot_preflight_check_sources_infer <- function(dcf) {

  # if this package appears to have a declared remote, use as-is
  for (field in c("RemoteType", "Repository", "biocViews"))
    if (!is.null(dcf[[field]]))
      return(NULL)

  # ok, this is a package installed from sources that "looks" like
  # the development version of a package; try to guess its remote
  guess <- function(pattern, field) {
    urls <- strsplit(dcf[[field]] %||% "", "\\s*,\\s*")[[1L]]
    for (url in urls) {
      matches <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1L]]
      if (length(matches) == 3L)
        return(paste(matches[[2L]], matches[[3L]], sep = "/"))
    }
  }

  # first, check bug reports
  remote <- guess("^https://(?:www\\.)?github\\.com/([^/]+)/([^/]+)/issues$", "BugReports")
  if (!is.null(remote))
    return(remote)

  # next, check the URL field
  remote <- guess("^https://(?:www\\.)?github\\.com/([^/]+)/([^/]+)", "URL")
  if (!is.null(remote))
    return(remote)

}

renv_snapshot_preflight_check_sources <- function(project, library) {

  # allow user to disable snapshot validation, just in case
  enabled <- config$snapshot.inference()
  if (!enabled)
    return(TRUE)

  # get package description files
  packages <- installed_packages(library, field = "Package")
  descpaths <- file.path(library, packages, "DESCRIPTION")
  dcfs <- lapply(descpaths, renv_description_read)
  names(dcfs) <- packages

  # try to infer sources as necessary
  inferred <- map(dcfs, renv_snapshot_preflight_check_sources_infer)
  inferred <- filter(inferred, Negate(is.null))
  if (length(inferred) == 0L)
    return(TRUE)

  # ask used
  renv_scope_options(renv.verbose = TRUE)
  renv_pretty_print(
    "The following package(s) were installed from sources, but may be available from the following remotes:",
    sprintf("%s  [%s]", format(names(inferred)), inferred),
    c(
      "Would you like to use these remote sources for these packages?",
      "See 'snapshot.inference' in ?renv::config for more details.",
      "Otherwise, these packages will be recorded with an unknown source."
    )
  )

  update <- ask("Allow renv to update the remote sources for these packages?", default = TRUE)
  if (!update)
    return(TRUE)

  enumerate(inferred, function(package, remote) {
    record <- renv_remotes_resolve(remote)
    record[["RemoteSha"]] <- NULL
    renv_package_augment(file.path(library, package), record)
  })

  TRUE

}

renv_snapshot_validate <- function(project, lockfile, libpaths) {

  # allow user to disable snapshot validation, just in case
  enabled <- config$snapshot.validate()
  if (!enabled)
    return(TRUE)

  methods <- list(
    renv_snapshot_validate_bioconductor,
    renv_snapshot_validate_dependencies_available,
    renv_snapshot_validate_dependencies_compatible,
    renv_snapshot_validate_sources
  )

  ok <- map_lgl(methods, function(method) {
    tryCatch(
      method(project, lockfile, libpaths),
      error = function(e) { warning(e); FALSE }
    )
  })

  all(ok)

}

renv_snapshot_validate_report <- function(valid, prompt, force) {

  # nothing to do if everything is valid
  if (valid) {
    dlog("snapshot", "passed pre-flight validation checks")
    return(TRUE)
  }

  # if we're forcing snapshot, ignore the failures
  if (force) {
    dlog("snapshot", "ignoring error in pre-flight validation checks as 'force = TRUE'")
    return(TRUE)
  }

  # in interactive sessions, if 'prompt' is set, then ask the user
  # if they would like to proceed
  if (interactive() && !is_testing() && prompt) {
    cancel_if(!proceed())
    return(TRUE)
  }

  # otherwise, bail on error (need to use 'force = TRUE')
  stop("aborting snapshot due to pre-flight validation failure")

}

# nocov start
renv_snapshot_validate_bioconductor <- function(project, lockfile, libpaths) {

  ok <- TRUE

  # check whether any packages are installed from Bioconductor
  records <- renv_lockfile_records(lockfile)
  sources <- extract_chr(records, "Source")
  if (!"Bioconductor" %in% sources)
    return(ok)

  # check for BiocManager or BiocInstaller
  package <- renv_bioconductor_manager()
  if (!package %in% names(records)) {

    text <- c(
      "One or more Bioconductor packages are used in your project,",
      "but the %s package is not available.",
      "",
      "Consider installing %s before snapshot.",
      ""
    )
    writef(text, package)

    ok <- FALSE
  }

  # check that Bioconductor packages are from correct release
  version <-
    lockfile$Bioconductor$Version %||%
    renv_bioconductor_version(project = project)

  biocrepos <- renv_bioconductor_repos(version = version)
  renv_scope_options(repos = biocrepos)

  # collect Bioconductor records
  bioc <- records %>%
    filter(function(record) renv_record_source(record) == "bioconductor") %>%
    map(function(record) record[c("Package", "Version")]) %>%
    bind()

  # collect latest versions of these packages
  bioc$Latest <- vapply(bioc$Package, function(package) {
    entry <- catch(renv_available_packages_latest(package))
    if (inherits(entry, "error"))
      return("<NA>")
    entry$Version
  }, FUN.VALUE = character(1))

  # check for version mismatches (allow mismatch in minor version)
  bioc$Mismatch <- mapply(function(current, latest) {

    if (identical(latest, "<NA>"))
      return(TRUE)

    current <- renv_version_maj_min(current)
    latest <- renv_version_maj_min(latest)
    current != latest

  }, bioc$Version, bioc$Latest)

  bad <- bioc[bioc$Mismatch, ]
  if (nrow(bad)) {

    fmt <- "%s [installed %s != latest %s]"
    msg <- sprintf(fmt, format(bad$Package), format(bad$Version), bad$Latest)
    renv_pretty_print(
      "The following Bioconductor packages appear to be from a separate Bioconductor release:",
      msg,
      c(
        "renv may be unable to restore these packages.",
        paste("Bioconductor version:", version)
      )
    )

    ok <- FALSE
  }

  ok

}
# nocov end

renv_snapshot_validate_dependencies_available <- function(project, lockfile, libpaths) {

  # use library to collect package dependency versions
  records <- renv_lockfile_records(lockfile)
  packages <- extract_chr(records, "Package")
  locs <- find.package(packages, lib.loc = libpaths, quiet = TRUE)
  deps <- bapply(locs, renv_dependencies_discover_description)
  if (empty(deps))
    return(TRUE)

  splat <- split(deps, deps$Package)

  # exclude base R packages
  splat <- splat[renv_vector_diff(names(splat), renv_packages_base())]

  # check for required packages not currently installed
  requested <- names(splat)
  missing <- renv_vector_diff(requested, packages)
  if (empty(missing))
    return(TRUE)

  # exclude ignored packages
  missing <- renv_vector_diff(missing, settings$ignored.packages(project = project))
  if (empty(missing))
    return(TRUE)

  usedby <- map_chr(missing, function(package) {

    revdeps <- sort(unique(basename(deps$Source)[deps$Package == package]))

    items <- revdeps; limit <- 3L
    if (length(revdeps) > limit) {
      rest <- length(revdeps) - limit
      suffix <- paste("and", length(revdeps) - 3L, plural("other", rest))
      items <- c(revdeps[seq_len(limit)], suffix)
    }

    paste(items, collapse = ", ")

  })

  renv_pretty_print(
    "The following required packages are not installed:",
    sprintf("%s  [required by %s]", format(missing), usedby),
    "Consider reinstalling these packages before snapshotting the lockfile."
  )

  FALSE

}

renv_snapshot_validate_dependencies_compatible <- function(project, lockfile, libpaths) {

  # use library to collect package dependency versions
  records <- renv_lockfile_records(lockfile)
  packages <- extract_chr(records, "Package")
  locs <- find.package(packages, lib.loc = libpaths, quiet = TRUE)
  deps <- bapply(locs, renv_dependencies_discover_description)
  if (empty(deps))
    return(TRUE)

  splat <- split(deps, deps$Package)

  # exclude base R packages
  splat <- splat[renv_vector_diff(names(splat), renv_packages_base())]

  # collapse requirements for each package
  bad <- enumerate(splat, function(package, requirements) {

    # skip NULL records (should be handled above)
    record <- records[[package]]
    if (is.null(record))
      return(NULL)

    version <- record$Version

    # drop packages without explicit version requirement
    requirements <- requirements[nzchar(requirements$Require), ]
    if (nrow(requirements) == 0)
      return(NULL)

    # add in requested version
    requirements$Requested <- version

    # generate expressions to evaluate
    fmt <- "package_version('%s') %s package_version('%s')"
    code <- with(requirements, sprintf(fmt, Requested, Require, Version))
    parsed <- parse(text = code)
    ok <- map_lgl(parsed, eval, envir = baseenv())

    # return requirements that weren't satisfied
    requirements[!ok, ]

  })

  bad <- bind(bad)
  if (empty(bad))
    return(TRUE)

  package  <- basename(bad$Source)
  requires <- sprintf("%s (%s %s)", bad$Package, bad$Require, bad$Version)
  request  <- bad$Requested

  fmt <- "%s requires %s, but version %s is installed"
  txt <- sprintf(fmt, format(package), format(requires), format(request))
  renv_pretty_print(
    "The following package(s) have unsatisfied dependencies:",
    txt,
    "Consider updating the required dependencies as appropriate."
  )

  FALSE

}

renv_snapshot_validate_sources <- function(project, lockfile, libpaths) {
  records <- renv_lockfile_records(lockfile)
  renv_check_unknown_source(records, project)
}

# NOTE: if packages are found in multiple libraries,
# then the first package found in the library paths is
# kept and others are discarded
renv_snapshot_libpaths <- function(libpaths = NULL,
                                   project  = NULL)
{
  dynamic(
    key   = list(libpaths = libpaths, project = project),
    value = renv_snapshot_libpaths_impl(libpaths, project)
  )
}

renv_snapshot_libpaths_impl <- function(libpaths = NULL,
                                        project  = NULL)
{
  records <- uapply(
    libpaths,
    renv_snapshot_library,
    project = project
  )

  dupes <- duplicated(names(records))
  records[!dupes]
}

renv_snapshot_library <- function(library = NULL,
                                  records = TRUE,
                                  project = NULL)
{
  # list packages in the library
  library <- renv_path_normalize(library %||% renv_libpaths_active())
  paths <- list.files(library, full.names = TRUE)

  # remove 'base' packages
  paths <- paths[!basename(paths) %in% renv_packages_base()]

  # remove ignored packages
  ignored <- renv_project_ignored_packages(project = project)
  paths <- paths[!basename(paths) %in% ignored]

  # remove paths that are not valid package names
  pattern <- sprintf("^%s$", .standard_regexps()$valid_package_name)
  paths <- paths[grep(pattern, basename(paths))]

  # validate the remaining set of packages
  valid <- renv_snapshot_library_diagnose(library, paths)

  # remove duplicates (so only first package entry discovered in library wins)
  duplicated <- duplicated(basename(valid))
  packages <- valid[!duplicated]

  # early exit if we're just collecting the list of packages
  if (!records)
    return(basename(packages))

  # snapshot description files
  descriptions <- file.path(packages, "DESCRIPTION")
  records <- lapply(descriptions, compose(catch, renv_snapshot_description))
  names(records) <- basename(packages)

  # report any snapshot failures
  broken <- filter(records, inherits, what = "error")
  if (length(broken)) {

    messages <- map_chr(broken, conditionMessage)
    text <- sprintf("'%s': %s", names(broken), messages)
    renv_pretty_print(
      "renv was unable to snapshot the following packages:",
      text,
      "These packages will likely need to be repaired and / or reinstalled."
    )

    stopf("snapshot of library %s failed", renv_path_pretty(library))

  }

  # name results and return
  names(records) <- map_chr(records, `[[`, "Package")
  records

}

renv_snapshot_library_diagnose <- function(library, paths) {

  paths <- grep("00LOCK", paths, invert = TRUE, value = TRUE)
  paths <- renv_snapshot_library_diagnose_broken_link(library, paths)
  paths <- renv_snapshot_library_diagnose_tempfile(library, paths)
  paths <- renv_snapshot_library_diagnose_missing_description(library, paths)
  paths

}

renv_snapshot_library_diagnose_broken_link <- function(library, paths) {

  broken <- !file.exists(paths)
  if (!any(broken))
    return(paths)

  renv_pretty_print(
    "The following package(s) have broken symlinks into the cache:",
    basename(paths)[broken],
    "Use `renv::repair()` to try and reinstall these packages."
  )

  paths[!broken]

}

renv_snapshot_library_diagnose_tempfile <- function(library, paths) {

  names <- basename(paths)
  missing <- grepl("^file(?:\\w){12}", names)
  if (!any(missing))
    return(paths)

  renv_pretty_print(
    "The following folder(s) appear to be left-over temporary directories:",
    map_chr(paths[missing], renv_path_pretty),
    "Consider removing these folders from your R library."
  )

  paths[!missing]

}

renv_snapshot_library_diagnose_missing_description <- function(library, paths) {

  desc <- file.path(paths, "DESCRIPTION")
  missing <- !file.exists(desc)
  if (!any(missing))
    return(paths)

  renv_pretty_print(
    "The following package(s) are missing their DESCRIPTION files:",
    sprintf("%s [%s]", format(basename(paths[missing])), paths[missing]),
    c(
      "These may be left over from a prior, failed installation attempt.",
      "Consider removing or reinstalling these packages."
    )
  )

  paths[!missing]

}

renv_snapshot_description <- function(path = NULL, package = NULL) {

  # resolve path
  path <- path %||% {
    path <- renv_package_find(package)
    if (!nzchar(path))
      stopf("package '%s' is not installed", package)
  }

  # read and snapshot DESCRIPTION file
  dcf <- renv_description_read(path, package)
  renv_snapshot_description_impl(dcf, path)

}

renv_snapshot_description_impl <- function(dcf, path = NULL) {

  # figure out the package source
  source <- renv_snapshot_description_source(dcf)
  dcf[names(source)] <- source

  # check for required fields
  required <- c("Package", "Version", "Source")
  missing <- renv_vector_diff(required, names(dcf))
  if (length(missing)) {
    fmt <- "required fields %s missing from DESCRIPTION at path '%s'"
    stopf(fmt, paste(shQuote(missing), collapse = ", "), path %||% "<unknown>")
  }

  # generate a hash if we can
  dcf[["Hash"]] <- if (the$snapshot_hash) {
    if (is.null(path))
      renv_hash_description_impl(dcf)
    else
      renv_hash_description(path)
  }

  # generate a Requirements field -- primarily for use by 'pak'
  fields <- c("Depends", "Imports", "LinkingTo")
  deps <- bind(map(dcf[fields], renv_description_parse_field))
  all <- unique(csort(unlist(deps$Package)))
  dcf[["Requirements"]] <- all

  # get remotes fields
  git <- grep("^git", names(dcf), value = TRUE)
  remotes <- grep("^Remote", names(dcf), value = TRUE)

  is_repo <-
    is.null(dcf[["RemoteType"]]) ||
    identical(dcf[["RemoteType"]], "standard")

  # only keep relevant fields
  extra <- c("Repository", "OS_type")
  all <- c(
    required, extra,
    if (!is_repo) c(remotes, git),
    "Requirements", "Hash"
  )
  keep <- renv_vector_intersect(all, names(dcf))

  # return as list
  as.list(dcf[keep])

}

renv_snapshot_description_source <- function(dcf) {

  # first, check for a declared remote type
  # treat 'standard' remotes as packages installed from a repository
  # https://github.com/rstudio/renv/issues/998
  type <- dcf[["RemoteType"]]
  repository <- dcf[["Repository"]]
  if (identical(type, "standard") && !is.null(repository))
    return(list(Source = "Repository", Repository = repository))
  else if (!is.null(type))
    return(list(Source = alias(type)))

  # packages from Bioconductor are normally tagged with a 'biocViews' entry;
  # use that to infer a Bioconductor source
  if (!is.null(dcf[["biocViews"]]))
    return(list(Source = "Bioconductor"))

  # check for a declared repository
  if (!is.null(repository))
    return(list(Source = "Repository", Repository = repository))

  # check for a valid package name
  package <- dcf[["Package"]]
  if (is.null(package))
    return(list(Source = "unknown"))

  # if this is running as part of the synchronization check, skip CRAN queries
  # https://github.com/rstudio/renv/issues/812
  if (the$project_synchronized_check_running)
    return(list(Source = "unknown"))

  # NOTE: this is sort of a hack that allows renv to declare packages which
  # appear to be installed from sources, but are actually available on the
  # active R package repositories, as though they were retrieved from that
  # repository. however, this is often what users intend, especially if
  # they haven't configured their repository to tag the packages it makes
  # available with the 'Repository:' field in the DESCRIPTION file.
  #
  # still, this has the awkward side-effect of a package's source potentially
  # depending on what repositories happen to be active at the time of snapshot,
  # so it'd be nice to tighten up the logic here if possible
  #
  # NOTE: local sources are also searched here as part of finding the 'latest'
  # available package, so we need to handle local packages discovered here
  tryCatch(
    renv_snapshot_description_source_hack(package, dcf),
    error = function(e) list(Source = "unknown")
  )

}

renv_snapshot_description_source_hack <- function(package, dcf) {

  # check cellar
  for (type in renv_package_pkgtypes()) {
    cellar <- renv_available_packages_cellar(type)
    if (package %in% cellar$Package)
      return(list(Source = "Cellar"))
  }

  # check available packages
  latest <- catch(renv_available_packages_latest(package))
  if (is.null(latest) || inherits(latest, "error"))
    return(list(Source = "unknown"))

  # check version; use unknown if it's too new
  if (renv_version_gt(dcf[["Version"]], latest[["Version"]]))
    return(list(Source = "unknown"))

  # ok, this package appears to be from a package repository
  list(Source = "Repository", Repository = latest[["Repository"]])

}


# nocov start
renv_snapshot_report_actions <- function(actions, old, new) {

  if (!renv_verbose())
    return(invisible())

  if (length(actions)) {
    lhs <- renv_lockfile_records(old)
    rhs <- renv_lockfile_records(new)
    renv_pretty_print_records_pair(
      "The following package(s) will be updated in the lockfile:",
      lhs[names(lhs) %in% names(actions)],
      rhs[names(rhs) %in% names(actions)]
    )
  }

  oldr <- old$R$Version
  newr <- new$R$Version
  rdiff <- renv_version_compare(oldr %||% "0", newr %||% "0")

  if (rdiff != 0L) {
    n <- max(nchar(names(actions)), 0)
    fmt <- paste("-", format("R", width = n), " ", "[%s -> %s]")
    msg <- sprintf(fmt, oldr %||% "*", newr %||% "*")
    writef(
      c("The version of R recorded in the lockfile will be updated:", msg, "")
    )
  }

}
# nocov end

# compute the package dependencies inferred for a project,
# respecting the snapshot type selected (or currently configured)
# for the associated project
renv_snapshot_dependencies <- function(project, type = NULL, dev = FALSE) {

  type <- type %||% settings$snapshot.type(project = project)

  packages <- dynamic(
    list(project = project, type = type, dev = dev),
    renv_snapshot_dependencies_impl(project, type, dev)
  )

  if (!renv_tests_running())
    packages <- unique(c(packages, "renv"))

  packages

}

renv_snapshot_dependencies_impl <- function(project, type = NULL, dev = FALSE) {

  if (type %in% "all") {
    packages <- installed_packages(field = "Package")
    return(setdiff(packages, renv_packages_base()))
  }

  if (type %in% "custom") {
    filter <- renv_snapshot_filter_custom_resolve()
    return(filter(project))
  }

  path <- case(
    type %in% c("packrat", "implicit") ~ project,
    type %in% "explicit" ~ file.path(project, "DESCRIPTION"),
    ~ {
      fmt <- "internal error: unhandled snapshot type '%s' in %s"
      stopf(fmt, type, stringify(sys.call()))
    }
  )

  packages <- withCallingHandlers(

    renv_dependencies_impl(
      path = path,
      root = project,
      field = "Package",
      errors = config$dependency.errors(),
      dev = dev
    ),

    # require user confirmation to proceed if there's a reported error
    renv.dependencies.problems = function(cnd) {

      if (identical(config$dependency.errors(), "ignored"))
        return()

      if (interactive() && !proceed())
        cancel()

    },

    # notify the user if we took a long time to discover dependencies
    renv.dependencies.elapsed_time = function(cnd) {

      # only relevant for implicit-type snapshots
      if (!type %in% c("packrat", "implicit"))
        return()

      # check for timeout
      elapsed <- cnd$data
      limit <- getOption("renv.dependencies.elapsed_time_threshold", default = 10L)
      if (elapsed < limit)
        return()

      # report to user
      lines <- c(
        "",
        "NOTE: Dependency discovery took %s during snapshot.",
        "Consider using .renvignore to ignore files, or switching to explicit snapshots.",
        "See `?renv::dependencies` for more information.",
        ""
      )

      writef(lines, renv_difftime_format(elapsed))

    }

  )

  unique(packages)

}

# compute package records from the provided library paths,
# normally to be included as part of an renv lockfile
renv_snapshot_packages <- function(packages, libpaths, project) {

  ignored <- c(
    renv_packages_base(),
    renv_project_ignored_packages(project = project),
    if (renv_tests_running()) "renv"
  )

  callback <- function(package, location, project) {
    if (nzchar(location) && !package %in% ignored)
      return(location)
  }

  # expand package dependency tree
  paths <- renv_package_dependencies(
    packages = packages,
    libpaths = libpaths,
    callback = callback,
    project = project
  )

  # keep only packages with known locations
  paths <- convert(filter(paths, is.character), "character")

  # diagnose issues with the scanned packages
  paths <- uapply(libpaths, function(library) {
    renv_snapshot_library_diagnose(
      library = library,
      paths   = filter(paths, startswith, prefix = library))
  })

  # now, snapshot the remaining packages
  records <- map(paths, renv_snapshot_description)

}

renv_snapshot_report_missing <- function(missing, type) {

  missing <- setdiff(missing, "renv")
  if (empty(missing))
    return(TRUE)

  preamble <- "The following required packages are not installed:"

  postamble <- c(
    "Packages must first be installed before renv can snapshot them.",
    if (type %in% "explicit")
      "If these packages are no longer required, consider removing them from your DESCRIPTION file."
    else
      "Use `renv::dependencies()` to see where this package is used in your project."
  )

  renv_pretty_print(
    preamble = preamble,
    values = sort(unique(missing)),
    postamble = postamble
  )

  # only prompt the user to install if a restart is available
  restart <- findRestart("renv_recompute_records")

  choices <- c(
    snapshot = "Snapshot, just using the currently installed packages.",
    install = if (isRestart(restart))
      "Install the packages, then snapshot.",
    cancel = "Cancel, and resolve the situation on your own."
  )

  choice <- menu(choices, title = "What do you want to do?")

  if (choice == "snapshot") {
    # do nothing
  } else if (choice == "install") {
    install(missing, prompt = FALSE)
    invokeRestart(restart)
  } else {
    cancel()
  }

  TRUE
}

renv_snapshot_filter_custom_resolve <- function() {

  # check for custom filter
  filter <- getOption("renv.snapshot.filter", default = NULL)
  if (is.null(filter)) {
    fmt <- "snapshot of type '%s' requested, but '%s' is not registered"
    stopf(fmt, "custom", "renv.snapshot.filter")
  }

  # allow for filter naming a function to use
  if (is.character(filter))
    filter <- eval(parse(text = filter), envir = baseenv())

  # check we got a function
  if (!is.function(filter)) {
    fmt <- "snapshot of type '%s' requested, but '%s' is not a function"
    stopf(fmt, "custom", "renv.snapshot.filter")
  }

  # return resolved function
  filter

}

renv_snapshot_fixup <- function(records) {

  records <- renv_snapshot_fixup_renv(records)
  records

}

renv_snapshot_fixup_renv <- function(records) {

  # don't run when testing renv
  if (renv_tests_running())
    return(records)

  # check for an existing valid record
  record <- records$renv
  if (is.null(record))
    return(records)

  source <- renv_record_source(record)
  if (source != "unknown")
    return(records)

  # no valid record available; construct a synthetic one
  remote <- renv_metadata_remote()

  # add it to the set of records
  records$renv <- renv_remotes_resolve(remote)

  # return it
  records

}

renv_snapshot_reprex <- function(lockfile) {

  fmt <- "<sup>Lockfile generated by renv %s.</sup>"
  version <- sprintf(fmt, renv_metadata_version_friendly())

  text <- c(
    "<details style=\"margin-bottom: 10px;\">",
    "<summary>Lockfile</summary>",
    "```",
    renv_lockfile_write(lockfile, file = NULL),
    "```",
    version,
    "</details>"
  )

  output <- paste(text, collapse = "\n")
  class(output) <- "knit_asis"
  attr(output, "knit_cacheable") <- NA

  output

}

renv_snapshot_successful <- function(records, prompt, project) {

  # update snapshot flag
  the$snapshot_failed <- FALSE

  # perform python snapshot on success
  renv_python_snapshot(project, prompt)

  # return generated records
  invisible(records)

}
