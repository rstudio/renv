
# the minimum-required version of 'pak' for renv integration
the$pak_minver <- numeric_version("0.9.0")

renv_pak_init <- function(stream = NULL, force = FALSE) {

  if (force || !renv_pak_available()) {
    stream <- stream %||% renv_pak_stream()
    renv_pak_init_impl(stream)
  }

  renv_namespace_load("pak")

}

renv_pak_stream <- function() {

  # check if stable is new enough
  streams <- c("stable", "rc", "devel")
  for (stream in streams) {
    repos <- renv_pak_repos(stream)
    latest <- renv_available_packages_latest("pak", repos = repos)
    version <- numeric_version(latest$Version)
    if (version >= the$pak_minver)
      return(stream)
  }

  fmt <- "internal error: pak (>= %s) is not available"
  stopf(fmt, format(the$pak_minver))

}

renv_pak_available <- function() {
  tryCatch(
    packageVersion("pak") >= the$pak_minver,
    error = function(e) FALSE
  )
}

renv_pak_repos <- function(stream) {

  # on macOS, we can only use pak binaries with CRAN R
  if (renv_platform_macos() && .Platform$pkgType == "source")
    return(getOption("repos"))

  # otherwise, use pre-built pak binaries
  fmt <- "https://r-lib.github.io/p/pak/%s/%s/%s/%s"
  sprintf(fmt, stream, .Platform$pkgType, version$os, version$arch)

}

renv_pak_init_impl <- function(stream) {

  renv_scope_options(
    renv.config.pak.enabled = FALSE,
    renv.config.ppm.enabled = FALSE,
    repos = c("r-lib" = renv_pak_repos(stream))
  )

  library <- renv_libpaths_active()
  install("pak", library = library)
  loadNamespace("pak", lib.loc = library)

}

renv_pak_update <- function(project, library, prompt) {
  
  pak <- renv_namespace_load("pak")

  # if this project contains a DESCRIPTION file, use it when
  # determining which packages to update
  if (file.exists(file.path(project, "DESCRIPTION"))) {
    
    result <- pak$local_install_dev_deps(
      root = project,
      lib  = library[[1L]],
      ask  = prompt
    )
    
    return(result)
  }
  
  # read description files for all installed packages
  # TODO: do we want to also update packages in other library paths,
  # or just packages installed in the project library?
  records <- renv_snapshot_libpaths(library[[1L]], project = project)
  remotes <- map_chr(records, renv_record_format_remote, versioned = FALSE, pak = TRUE)
  if (length(remotes) == 0L) {
    caution("- There are no packages to update.")
    return(invisible(NULL))
  }
  
  # update those packages
  pak$pkg_install(
    pkg = unname(remotes),
    lib = library[[1L]],
    upgrade = TRUE,
    ask = prompt
  )

}

renv_pak_install <- function(packages,
                             library,
                             type,
                             rebuild,
                             prompt,
                             project,
                             include = NULL,
                             exclude = NULL)
{
  pak <- renv_namespace_load("pak")

  # transform repositories
  if (renv_ppm_enabled()) {
    repos <- getOption("repos")
    renv_scope_options(repos = renv_ppm_transform(repos))
  }

  # make sure pak::pkg_install() still works even if we're
  # running in renv with devtools::load_all()
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    renv_scope_envvars("_R_CHECK_PACKAGE_NAME_" = NULL)

  # convert the requested packages into pak-compatible remote specifications.
  # some callers (e.g. renv::use(), rebuild(), repair()) hand us a named list
  # of already-resolved records; using those names alone would discard the
  # record's version pin and remote source
  # https://github.com/rstudio/renv/issues/2341
  specs <- map_chr(packages, function(package) {
    if (is.list(package))
      renv_record_format_remote(package, pak = TRUE)
    else
      as.character(package)
  })

  # associate each spec with its package name where we know it, so include /
  # exclude filter by package name rather than by remote specification
  nms <- names(packages) %||% rep.int("", length(specs))
  names(specs) <- ifelse(nzchar(nms), nms, specs)

  # remember whether the caller explicitly scoped this install so we can
  # distinguish "no scope at all" (let pak update the project) from "explicit
  # scope filtered to empty" (no-op, matching the non-pak path)
  explicit <- length(specs) > 0L || length(include) > 0L || length(exclude) > 0L

  # apply include / exclude consistently with the non-pak install path,
  # so the semantics of these arguments don't depend on the installer.
  # if no packages were specified positionally but include was, treat it
  # as the request set (e.g. install(include = ...))
  # https://github.com/rstudio/renv/issues/2281
  if (length(specs) == 0L && length(include)) {
    specs <- as.character(include)
    names(specs) <- specs
  }

  if (length(exclude))
    specs <- specs[!names(specs) %in% exclude]

  if (length(include))
    specs <- specs[names(specs) %in% include]

  # if no packages remain, fall through to a project-wide update only when
  # the caller did not provide an explicit scope; otherwise treat this as
  # a no-op so we don't surprise the user with an unintended update
  if (length(specs) == 0L) {
    if (explicit) {
      writef("- There are no packages to install.")
      return(invisible(list()))
    }
    return(renv_pak_update(project, library, prompt))
  }

  # pak doesn't support ':' as a sub-directory separator, so try to
  # repair that here
  # https://github.com/rstudio/renv/issues/2011
  pattern <- "(?<!:):([^/#@:]+)"
  packages <- gsub(pattern, "/\\1", unname(specs), perl = TRUE)
  
  # build parameters. explicitly-requested packages always get 'reinstall',
  # so they are installed even when already current -- matching renv's non-pak
  # installer, which always (re)installs explicitly-requested packages. with
  # 'upgrade = FALSE', pak then leaves transitive dependencies (including
  # recommended packages like 'cluster') at their installed version unless a
  # dependency constraint requires otherwise. https://github.com/rstudio/renv/issues/2329
  packages <- map_chr(packages, function(package) {

    params <- c(
      if (identical(type, "source")) "source",
      "reinstall"
    )

    # pak (pkgdepends) reads everything after the first '?' as an '&'-separated
    # query; append with '&' if the spec already carries a query, '?' otherwise
    sep <- if (grepl("?", package, fixed = TRUE)) "&" else "?"
    paste(package, paste(params, collapse = "&"), sep = sep)

  })

  pak$pkg_install(
    pkg     = packages,
    lib     = library[[1L]],
    ask     = prompt,
    upgrade = FALSE
  )
}

renv_pak_restore <- function(lockfile,
                             packages = NULL,
                             exclude = NULL,
                             prompt = FALSE,
                             project = NULL)
{
  pak <- renv_namespace_load("pak")

  # transform repositories
  if (renv_ppm_enabled()) {
    repos <- getOption("repos")
    renv_scope_options(repos = renv_ppm_transform(repos))
  }

  # make sure pak::pkg_install() still works even if we're
  # running in renv with devtools::load_all()
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    renv_scope_envvars("_R_CHECK_PACKAGE_NAME_" = NULL)

  # get records to install
  records <- renv_lockfile_records(lockfile)
  packages <- setdiff(packages %||% names(records), c(exclude, "pak", "renv"))
  records <- records[packages]

  # convert into specs compatible with pak, and install
  remotes <- map_chr(records, renv_record_format_remote, pak = TRUE)

  # TODO: We previously tried converting version-ed remotes into "plain" remotes
  # if the package version happened to be current, but then 'pak' would choose
  # not to install the package if a newer version was available. Hence, we need
  # to preserve the exact remote we wish to install here.

  # return early if there are zero remotes to restore
  if (length(remotes) == 0L) {
    return(invisible(TRUE))
  }

  # perform installation
  pak$pkg_install(
    pkg = remotes,
    ask = prompt
  )

  # return installed records
  records

}

renv_pak_restore_clean <- function(lockfile,
                                   libpaths,
                                   library,
                                   project,
                                   packages,
                                   exclude,
                                   prompt)
{
  current <- snapshot(
    project  = project,
    library  = libpaths,
    lockfile = NULL,
    type     = "all"
  )

  diff <- renv_lockfile_diff_packages(current, lockfile)
  removes <- diff[diff == "remove"]
  if (!length(removes))
    return(invisible(NULL))

  # only remove packages from the project library
  ispkg <- map_lgl(names(removes), function(package) {
    path <- find.package(package, lib.loc = libpaths, quiet = TRUE)
    identical(dirname(path), library)
  })
  removes <- removes[ispkg]

  # don't remove ignored packages
  ignored <- renv_project_ignored_packages(project = project)
  removes <- removes[renv_vector_diff(names(removes), ignored)]

  # restrict to user-requested packages. unlike the main restore() path we
  # don't expand `packages` via renv_graph_init() here, since the install
  # graph's transitive deps come from the lockfile and so won't appear in
  # the remove diff anyway.
  selected <- if (is.null(packages))
    setdiff(names(removes), exclude)
  else
    setdiff(packages, exclude)
  removes <- removes[intersect(names(removes), selected)]

  if (!length(removes))
    return(invisible(NULL))

  # report planned removals and confirm before mutating the library
  if (prompt || renv_verbose()) {
    renv_restore_report_actions(removes, current, lockfile)
    cancel_if(prompt && !proceed())
  }

  enumerate(removes, function(package, action) {
    renv_restore_remove(project, package, current)
  })

  invisible(removes)
}
