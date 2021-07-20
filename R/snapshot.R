
#' Snapshot a Project
#'
#' Call `snapshot()` to create a **lockfile** capturing the state of a project's
#' \R package dependencies. The lockfile can be used to later restore these
#' project's dependencies as required.
#'
#' See the [lockfile] documentation for more details on the structure of a
#' lockfile.
#'
#' @section Snapshot Type:
#'
#' Depending on how you prefer to manage dependencies, you might prefer
#' selecting a different snapshot mode. The modes available are as follows:
#'
#' \describe{
#'
#' \item{`"all"`}{
#' Capture all packages within the active \R libraries in the lockfile.
#' This is the quickest and simplest method, but may lead to undesired
#' packages (e.g. development dependencies) entering the lockfile.
#' }
#'
#' \item{`"implicit"`}{
#' Only capture packages which appear to be used in your project in the
#' lockfile. The intersection of packages installed in your \R libraries,
#' alongside those used in your \R code as inferred by `renv::dependencies()`,
#' will enter the lockfile. This helps ensure that only the packages your
#' project requires will enter the lockfile, but may be slower if your project
#' contains a large number of files. If this becomes an issue, you might
#' consider using `.renvignore` files to limit which files `renv` uses for
#' dependency discovery, or explicitly declaring your required dependencies in a
#' `DESCRIPTION` file. You can also force a dependency on a particular package
#' by writing e.g. `library(<package>)` into a file called `dependencies.R`.
#' }
#'
#' \item{`"explicit"`}{
#' Only capture packages which are explicitly listed in the project
#' `DESCRIPTION` file. This workflow is recommended for users who wish to
#' manage their project's \R package dependencies directly.
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
#' By default, `"implicit"`-style snapshots are used. The snapshot type can be
#' configured on a project-specific basis using the `renv` project [settings]
#' mechanism. For example, to use `"explicit"` snapshots in a project:
#'
#' ```
#' renv::settings$snapshot.type("explicit")
#' ````
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
#' @param type The type of snapshot to perform. See **Snapshot Type** for
#'   more details.
#'
#' @param packages A vector of packages to be included in the lockfile. When
#'   `NULL` (the default), all packages relevant for the type of snapshot being
#'   performed will be included. When set, the `type` argument is ignored.
#'   Recursive dependencies of the specified packages will be added to the
#'   lockfile as well.
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
                     packages = NULL,
                     prompt   = interactive(),
                     force    = FALSE,
                     reprex   = FALSE)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_snapshot_auto_suppress_next()

  project <- renv_project_resolve(project)
  renv_scope_lock(project = project)

  renv_activate_prompt("snapshot", library, prompt, project)

  libpaths <- library %||% renv_libpaths_all()
  if (config$snapshot.validate())
    renv_snapshot_preflight(project, libpaths)

  # when packages is set, we treat this as an 'all' type snapshot, but
  # with explicit package filters turned on
  if (!is.null(packages)) {

    if (!missing(type)) {
      fmt <- "packages argument is set; type argument %s will be ignored"
      warningf(fmt, renv_deparse(type))
    }

    type <- "packages"

  }

  alt <- new <- renv_lockfile_create(project, libpaths, type, packages)
  if (is.null(lockfile))
    return(new)

  # if running as part of 'reprex', then render output inline
  if (reprex)
    return(renv_snapshot_reprex(new))

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
      vwritef("* The lockfile is already up to date.")
      return(renv_snapshot_successful(alt, prompt, project))
    }

  }

  # check for missing dependencies and warn if any are discovered
  # (note: use 'new' rather than 'alt' here as we don't want to attempt
  # validation on uninstalled packages)
  validated <- renv_snapshot_validate(project, new, libpaths)
  if (!validated && !force) {
    if (prompt && !proceed()) {
      renv_report_user_cancel()
      return(invisible(alt))
    } else if (!interactive()) {
      stop("aborting snapshot due to pre-flight validation failure")
    }
  }

  # update new reference
  new <- alt

  # report actions to the user
  actions <- renv_lockfile_diff_packages(old, new)
  if (prompt || renv_verbose())
    renv_snapshot_report_actions(actions, old, new)

  # request user confirmation

  # nocov start
  if (length(actions) && prompt && !proceed()) {
    renv_report_user_cancel()
    return(invisible(new))
  }
  # nocov end

  # write it out
  ensure_parent_directory(lockfile)
  renv_lockfile_write(new, file = lockfile)
  vwritef("* Lockfile written to '%s'.", aliased_path(lockfile))

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
    stopf(fmt, aliased_path(library))
  }

  # the directory doesn't exist; perhaps the user hasn't called init
  if (identical(library, renv_paths_library(project = project))) {
    fmt <- "project '%s' has no private library -- have you called `renv::init()`?"
    stopf(fmt, aliased_path(project))
  }

  # user tried to snapshot arbitrary but missing path
  fmt <- "library '%s' does not exist; cannot proceed"
  stopf(fmt, aliased_path(library))

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

# nocov start
renv_snapshot_validate_bioconductor <- function(project, lockfile, libpaths) {

  ok <- TRUE

  # check whether any packages are installed from Bioconductor
  records <- renv_records(lockfile)
  sources <- extract_chr(records, "Source")
  if (!"Bioconductor" %in% sources)
    return(ok)

  # check for BiocManager or BiocInstaller
  package <- if (getRversion() >= "3.5.0") "BiocManager" else "BiocInstaller"
  if (!package %in% names(records)) {

    text <- c(
      "One or more Bioconductor packages are used in your project,",
      "but the %s package is not available.",
      "",
      "Consider installing %s before snapshot.",
      ""
    )

    if (!renv_tests_running())
      writeLines(sprintf(text, package))

    ok <- FALSE
  }

  # check that Bioconductor packages are from correct release
  version <-
    lockfile$Bioconductor$Version %||%
    renv_bioconductor_version()

  renv_scope_options(repos = renv_bioconductor_repos(version = version))

  # collect Bioconductor records
  bioc <- records %>%
    filter(function(record) renv_record_source(record) == "bioconductor") %>%
    map(function(record) record[c("Package", "Version")]) %>%
    bind_list()

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

    current <- unclass(package_version(current))[[1]]
    latest <- unclass(package_version(latest))[[1]]
    current[[1]] != latest[[1]] || current[[2]] != latest[[2]]

  }, bioc$Version, bioc$Latest)

  bad <- bioc[bioc$Mismatch, ]
  if (nrow(bad)) {

    fmt <- "%s [installed %s != latest %s]"
    msg <- sprintf(fmt, format(bad$Package), format(bad$Version), bad$Latest)

    if (!renv_tests_running()) {
      renv_pretty_print(
        msg,
        "The following Bioconductor packages appear to be from a separate Bioconductor release:",
        c(
          "renv may be unable to restore these packages.",
          paste("Bioconductor version:", version)
        ),
        wrap = FALSE
      )
    }

    ok <- FALSE
  }

  ok

}
# nocov end

renv_snapshot_validate_dependencies_available <- function(project, lockfile, libpaths) {

  # use library to collect package dependency versions
  records <- renv_records(lockfile)
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

    items <- revdeps; limit <- 3
    if (length(revdeps) > limit) {
      rest <- length(revdeps) - limit
      suffix <- paste("and", length(revdeps) - 3, plural("other", rest))
      items <- c(revdeps[seq_len(limit)], suffix)
    }

    paste(items, collapse = ", ")

  })

  if (!renv_tests_running()) {
    renv_pretty_print(
      sprintf("%s  [required by %s]", format(missing), usedby),
      "The following required packages are not installed:",
      "Consider reinstalling these packages before snapshotting the lockfile.",
      wrap = FALSE
    )
  }

  FALSE

}

renv_snapshot_validate_dependencies_compatible <- function(project, lockfile, libpaths) {

  # use library to collect package dependency versions
  records <- renv_records(lockfile)
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

  bad <- bind_list(bad)
  if (empty(bad))
    return(TRUE)

  package  <- basename(bad$Source)
  requires <- sprintf("%s (%s %s)", bad$Package, bad$Require, bad$Version)
  request  <- bad$Requested

  fmt <- "'%s' requires '%s', but version '%s' will be snapshotted"
  txt <- sprintf(fmt, format(package), format(requires), format(request))

  if (!renv_tests_running()) {
    renv_pretty_print(
      txt,
      "The following package(s) have unsatisfied dependencies:",
      "Consider updating the required dependencies as appropriate.",
      wrap = FALSE
    )
  }

  renv_condition_signal("renv.snapshot.unsatisfied_dependencies")
  FALSE

}

renv_snapshot_validate_sources <- function(project, lockfile, libpaths) {
  records <- renv_records(lockfile)
  renv_check_unknown_source(records, project)
}

# NOTE: if packages are found in multiple libraries,
# then the first package found in the library paths is
# kept and others are discarded
renv_snapshot_r_packages <- function(libpaths = NULL,
                                     project  = NULL)
{
  records <- uapply(
    libpaths,
    renv_snapshot_r_packages_impl,
    project = project
  )

  dupes <- duplicated(names(records))
  records[!dupes]
}

renv_snapshot_r_packages_impl <- function(library = NULL,
                                          project = NULL)
{
  # list packages in the library
  library <- renv_path_normalize(library %||% renv_libpaths_default())
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
  valid <- renv_snapshot_r_library_diagnose(library, paths)

  # remove duplicates (so only first package entry discovered in library wins)
  duplicated <- duplicated(basename(valid))
  packages <- valid[!duplicated]

  # snapshot description files
  descriptions <- file.path(packages, "DESCRIPTION")
  records <- lapply(descriptions, renv_snapshot_description)
  names(records) <- basename(packages)

  # report any snapshot failures
  broken <- Filter(function(record) inherits(record, "error"), records)
  if (length(broken)) {

    messages <- map_chr(broken, conditionMessage)
    text <- sprintf("'%s': %s", names(broken), messages)

    renv_pretty_print(
      text,
      "renv was unable to snapshot the following packages:",
      "These packages will likely need to be repaired and / or reinstalled."
    )

    stopf("snapshot of library %s failed", renv_path_pretty(library))

  }

  # name results and return
  names(records) <- map_chr(records, `[[`, "Package")
  records

}

renv_snapshot_r_library_diagnose <- function(library, pkgs) {

  pkgs <- grep("00LOCK", pkgs, invert = TRUE, value = TRUE)
  pkgs <- renv_snapshot_r_library_diagnose_broken_link(library, pkgs)
  pkgs <- renv_snapshot_r_library_diagnose_tempfile(library, pkgs)
  pkgs <- renv_snapshot_r_library_diagnose_missing_description(library, pkgs)
  pkgs

}

renv_snapshot_r_library_diagnose_broken_link <- function(library, pkgs) {

  broken <- !file.exists(pkgs)
  if (!any(broken))
    return(pkgs)

  renv_pretty_print(
    basename(pkgs)[broken],
    "The following package(s) have broken symlinks into the cache:",
    "Consider reinstalling these packages."
  )

  pkgs[!broken]

}

renv_snapshot_r_library_diagnose_tempfile <- function(library, pkgs) {

  names <- basename(pkgs)
  missing <- grepl("^file(?:\\w){12}", names)
  if (!any(missing))
    return(pkgs)

  renv_pretty_print(
    basename(pkgs)[missing],
    "The following folder(s) appear to be left-over temporary directories:",
    "Consider removing these folders from your library."
  )

  pkgs[!missing]

}

renv_snapshot_r_library_diagnose_missing_description <- function(library, pkgs) {

  desc <- file.path(pkgs, "DESCRIPTION")
  missing <- !file.exists(desc)
  if (!any(missing))
    return(pkgs)

  renv_pretty_print(
    sprintf("%s [%s]", format(basename(pkgs[missing])), pkgs[missing]),
    "The following package(s) are missing their DESCRIPTION files:",
    c(
      "These may be left over from a prior, failed installation attempt.",
      "Consider removing or reinstalling these packages."
    ),
    wrap = FALSE
  )

  pkgs[!missing]

}

renv_snapshot_description <- function(path = NULL, package = NULL) {

  path <- path %||% renv_package_find(package)
  dcf <- catch(renv_description_read(path, package))
  if (inherits(dcf, "error"))
    return(dcf)

  source <- renv_snapshot_description_source(dcf)
  dcf[names(source)] <- source
  dcf[["Hash"]] <- renv_hash_description(path)

  fields <- c("Package", "Version", "Source")
  missing <- renv_vector_diff(fields, names(dcf))
  if (length(missing)) {
    fmt <- "required fields %s missing from DESCRIPTION at path '%s'"
    msg <- sprintf(fmt, paste(shQuote(missing), collapse = ", "), path)
    return(simpleError(msg))
  }

  remotes <- grep("^Remote", names(dcf), value = TRUE)
  all <- c(fields, "Repository", "OS_type", remotes, "Hash")
  keep <- renv_vector_intersect(all, names(dcf))
  as.list(dcf[keep])

}

renv_snapshot_description_source <- function(dcf) {

  type <- dcf[["RemoteType"]]
  if (!is.null(type))
    return(list(Source = renv_alias(type)))

  if (!is.null(dcf[["Repository"]]))
    return(list(Source = "Repository", Repository = dcf[["Repository"]]))

  if (!is.null(dcf[["biocViews"]]))
    return(list(Source = "Bioconductor"))

  package <- dcf[["Package"]]
  if (is.null(package))
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
  entry <- local({
    renv_scope_options(renv.verbose = FALSE)
    catch(renv_available_packages_latest(package))
  })

  if (!inherits(entry, "error")) {

    # check for and handle local repositories
    repos <- entry[["Repository"]]
    if (identical(repos, "Local"))
      return(list(Source = "Local"))

    # otherwise, treat as regular entry
    return(list(Source = "Repository", Repository = repos))

  }

  location <- catch(renv_retrieve_local_find(dcf))
  if (!inherits(location, "error"))
    return(list(Source = "Local"))

  list(Source = "unknown")

}

# nocov start
renv_snapshot_report_actions <- function(actions, old, new) {

  if (!renv_verbose() || empty(actions))
    return(invisible())

  lhs <- renv_records(old)
  rhs <- renv_records(new)
  renv_pretty_print_records_pair(
    lhs[names(lhs) %in% names(actions)],
    rhs[names(rhs) %in% names(actions)],
    "The following package(s) will be updated in the lockfile:"
  )

}
# nocov end

renv_snapshot_dependencies <- function(project, source) {

  message <- "snapshot aborted"
  errors <- config$dependency.errors()

  withCallingHandlers(

    dependencies(
      path = source,
      root = project,
      progress = FALSE,
      errors = errors
    ),

    renv.dependencies.error = renv_dependencies_error_handler(message, errors)

  )

}

renv_snapshot_filter <- function(project, records, type, packages) {

  start <- Sys.time()

  type <- type %||% settings$snapshot.type(project = project)

  aliases <- list(packrat = "implicit", simple = "all")
  type <- aliases[[type]] %||% type

  result <- switch(type,
    all      = renv_snapshot_filter_all(project, records),
    custom   = renv_snapshot_filter_custom(project, records),
    explicit = renv_snapshot_filter_explicit(project, records),
    implicit = renv_snapshot_filter_implicit(project, records),
    packages = renv_snapshot_filter_packages(project, records, packages),
    stopf("unknown snapshot type '%s'", type)
  )

  if (type %in% c("all", "explicit"))
    return(result)

  end <- Sys.time()

  # report if dependency discovery took a long time
  limit <- getOption("renv.snapshot.filter.timelimit", default = 10L)
  diff <- difftime(end, start, units = "secs")

  if (diff > limit) {

    lines <- c(
      "NOTE: Dependency discovery took %s %s during snapshot.",
      "Consider using .renvignore to ignore files -- see `?dependencies` for more information.",
      "Use `renv::settings$snapshot.type(\"all\")` to disable dependency discovery during snapshot."
    )

    time <- difftime(end, start, units = "auto")
    elapsed <- format(unclass(signif(time, digits = 2L)))
    units <- switch(
      attr(time, "units"),
      secs  = "seconds",
      mins  = "minutes",
      hours = "hours",
      days  = "days",
      weeks = "weeks"
    )

    vwritef(lines, elapsed, units)

  }

  result

}

renv_snapshot_filter_all <- function(project, records) {
  records
}

renv_snapshot_filter_impl <- function(project, records, source) {

  deps <- renv_snapshot_dependencies(project, source)
  packages <- unique(c(deps$Package, "renv"))

  # ignore packages as defined by project
  ignored <- renv_project_ignored_packages(project = project)
  used <- setdiff(packages, ignored)

  # include transitive dependencies
  paths <- renv_package_dependencies(used, project = project)
  all <- as.character(names(paths))
  kept <- keep(records, all)

  # add in bioconductor infrastructure packages
  # if any other bioconductor packages detected
  sources <- extract_chr(kept, "Source")
  if ("Bioconductor" %in% sources) {
    packages <- c("BiocManager", "BiocInstaller", "BiocVersion")
    for (package in packages)
      kept[[package]] <- records[[package]]
  }

  kept

}

renv_snapshot_filter_implicit <- function(project, records) {
  renv_snapshot_filter_impl(project, records, project)
}

renv_snapshot_filter_explicit <- function(project, records) {

  # keep only packages mentioned in the project DESCRIPTION file
  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath)) {
    fmt <- "%s does not exist; cannot perform explicit snapshot"
    stopf(fmt, renv_path_pretty(descpath))
  }

  renv_snapshot_filter_impl(project, records, descpath)

}

renv_snapshot_filter_packages <- function(project, records, packages) {

  # include transitive dependencies
  paths <- renv_package_dependencies(packages, project = project)
  all <- as.character(names(paths))
  kept <- keep(records, all)

  # add in bioconductor infrastructure packages
  # if any other bioconductor packages detected
  sources <- extract_chr(kept, "Source")
  if ("Bioconductor" %in% sources) {
    packages <- c("BiocManager", "BiocInstaller", "BiocVersion")
    for (package in packages)
      kept[[package]] <- records[[package]]
  }

  kept

}

renv_snapshot_filter_custom <- function(project, records) {

  option <- "renv.snapshot.filter"
  filter <- getOption(option, default = NULL)

  # allow for filter naming a function to use
  if (is.character(filter))
    filter <- eval(parse(text = filter), envir = baseenv())

  # check we got a function
  if (!is.function(filter)) {
    fmt <- "snapshot of type '%s' requested, but '%s' is not a function"
    stopf(fmt, "custom", option)
  }

  # invoke the filter to get package list
  packages <- filter(project)

  if (empty(packages))
    return(records)

  if (!is.character(packages))
    stop("custom snapshot filter did not return a character vector")

  keep(records, packages)

}

renv_snapshot_fixup <- function(records) {

  records <- renv_snapshot_fixup_renv(records)
  records

}

renv_snapshot_fixup_renv <- function(records) {

  if (renv_tests_running())
    return(records)

  # nocov start
  record <- records$renv
  if (is.null(record))
    return(records)

  source <- renv_record_source(record)
  if (source != "unknown")
    return(records)

  remote <- paste("rstudio/renv", record$Version, sep = "@")
  records$renv <- renv_remotes_resolve(remote)
  records
  # nocov end

}

renv_snapshot_reprex <- function(lockfile) {

  fmt <- "<sup>Lockfile generated by renv %s.</sup>"
  version <- sprintf(fmt, renv_package_version("renv"))

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

  # perform python snapshot on success
  renv_python_snapshot(project, prompt)

  # return generated records
  invisible(records)

}
