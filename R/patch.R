
renv_patch_init <- function() {
  renv_patch_rprofile()
  renv_patch_tar()
  renv_patch_repos()
  renv_patch_golem()
  renv_patch_methods_table()
}

renv_patch_rprofile <- function() {

  # resolve path to user profile
  path <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
  info <- renv_file_info(path)
  if (!identical(info$isdir, FALSE))
    return(FALSE)

  # if the .Rprofile is empty, do nothing
  if (info$size == 0)
    return(TRUE)

  # check for trailing newline
  data <- readBin(path, raw(), n = info$size)
  if (empty(data))
    return(TRUE)

  last <- data[length(data)]
  endings <- as.raw(c(0x0a, 0x0d))
  if (last %in% endings)
    return(TRUE)

  # if it's missing, inform the user
  warningf("%s is missing a trailing newline", renv_path_pretty(path))
  FALSE

}

renv_patch_tar <- function() {

  # read value of TAR
  tar <- Sys.getenv("TAR", unset = "")

  # on Windows, if TAR is unset, then force the usage
  # of R's internal tar implementation. this is done to
  # avoid issues where e.g. versions of tar which do not
  # understand Windows paths are on the PATH
  #
  # https://github.com/rstudio/renv/issues/521
  if (renv_platform_windows() && !nzchar(tar)) {
    Sys.setenv(TAR = "internal")
    return(TRUE)
  }

  # otherwise, allow empty / internal tars
  if (tar %in% c("", "internal"))
    return(TRUE)

  # the user (or R itself) has set the TAR environment variable
  # validate that it exists (resolve from PATH)
  #
  # note that the user can set TAR to be a full command; e.g.
  #
  #    TAR = /path/to/tar --force-local
  #
  # so we need to handle that case appropriately
  whitespace <- gregexpr("(?:\\s+|$)", tar, perl = TRUE)[[1L]]
  for (index in whitespace) {
    candidate <- substring(tar, 1L, index - 1L)
    resolved <- Sys.which(candidate)
    if (nzchar(resolved))
      return(TRUE)
  }

  # TAR appears to be set but invalid; override it
  # and warn the user
  newtar <- Sys.which("tar")
  if (!nzchar(newtar))
    newtar <- "internal"

  Sys.setenv(TAR = newtar)

  # report to the user
  fmt <- "requested TAR '%s' does not exist; using '%s' instead"
  warningf(fmt, tar, newtar)

}

renv_patch_golem <- function() {
  renv_package_hook("golem", renv_patch_golem_impl)
}

renv_patch_golem_impl <- function(...) {

  if (packageVersion("golem") != "0.2.1")
    return()

  golem <- getNamespace("golem")

  replacement <- function(file, pattern, replace) {

    # skip .rds files
    if (grepl("[.]rds$", file))
      return()

    # skip files containing nul bytes
    info <- renv_file_info(file)
    bytes <- readBin(file, "raw", info$size)
    if (any(bytes == 0L))
      return()

    # otherwise, attempt replacement
    old <- readLines(file)
    new <- gsub(pattern, replace, old)
    writeLines(new, con = file)

  }

  environment(replacement) <- golem

  if ("compiler" %in% loadedNamespaces())
    replacement <- compiler::cmpfun(replacement)

  renv_binding_replace(golem, "replace_word", replacement)

}

renv_patch_methods_table <- function() {
  catchall(renv_patch_methods_table_impl())
}

renv_patch_methods_table_impl <- function() {

  # ensure promises in S3 methods table are forced
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16644
  for (envir in list(.BaseNamespaceEnv, renv_namespace_load("utils"))) {

    # unlock binding if it's locked
    binding <- ".__S3MethodsTable__."
    base <- baseenv()
    if (base$bindingIsLocked(binding, env = envir)) {
      base$unlockBinding(binding, env = envir)
      defer(base$lockBinding(binding, envir))
    }

    # force everything defined in the environment
    table <- envir[[binding]]
    for (key in ls(envir = table, all.names = TRUE))
      table[[key]] <- force(table[[key]])

  }

}

# puts the current version of renv into an on-disk package repository,
# so that packages using renv can find this version of renv in tests
# this helps renv survive CRAN revdep checks (e.g. jetpack)
renv_patch_repos <- function() {

  # nothing to do in embedded mode
  if (renv_metadata_embedded())
    return()

  # nothing to do if we're not running tests
  checking <- checking()
  if (!checking)
    return()

  # nothing to do if we're running our own tests
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    return()

  # presumably this will never happen when the dev version of renv is
  # installed, so we skip to avoid parsing a sha as version
  sha <- attr(the$metadata$version, "sha")
  if (!is.null(sha))
    return()

  # nothing to do if this version of 'renv' is already available
  version <- renv_metadata_version()
  entry <- catch(renv_available_packages_entry("renv", filter = version, quiet = TRUE))
  if (!inherits(entry, "error"))
    return()

  # check if we've already set repos
  if ("RENV" %in% names(getOption("repos")))
    return()

  # use package-local repository path
  repopath <- system.file("repos", package = "renv", mustWork = FALSE)
  if (!file.exists(repopath))
    return()

  # update our repos option
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  repourl <- sprintf(fmt, repopath)

  # renv needs to be first so the right version is found?
  repos <- c(RENV = repourl, getOption("repos"))
  names(repos) <- make.names(names(repos))
  options(repos = repos)

  # make sure these repositories are used in restore too
  options(renv.config.repos.override = repos)

}
