
renv_rtools_list <- function() {

  drive <- Sys.getenv("SYSTEMDRIVE", unset = "C:")

  roots <- c(

    renv_rtools_registry(),

    Sys.getenv("RTOOLS45_HOME", unset = file.path(drive, "rtools45")),
    Sys.getenv("RTOOLS44_HOME", unset = file.path(drive, "rtools44")),
    Sys.getenv("RTOOLS43_HOME", unset = file.path(drive, "rtools43")),
    Sys.getenv("RTOOLS42_HOME", unset = file.path(drive, "rtools42")),
    Sys.getenv("RTOOLS40_HOME", unset = file.path(drive, "rtools40")),
    file.path(drive, "Rtools"),
    list.files(file.path(drive, "RBuildTools"), full.names = TRUE),

    "~/Rtools",
    list.files("~/RBuildTools", full.names = TRUE)

  )

  roots <- unique(roots[file.exists(roots)])
  lapply(roots, renv_rtools_read)

}

renv_rtools_find <- function() {

  for (spec in renv_rtools_list())
    if (renv_rtools_compatible(spec))
      return(spec)

  NULL

}

renv_rtools_read <- function(root) {

  list(
    root    = root,
    version = renv_rtools_version(root)
  )

}

renv_rtools_version <- function(root) {

  name <- basename(root)

  # check for 'rtools<xyz>' folder
  # e.g. C:/rtools42
  pattern <- "^rtools(\\d)(\\d)$"
  if (grepl(pattern, name, perl = TRUE, ignore.case = TRUE))
    return(gsub(pattern, "\\1.\\2", name, perl = TRUE, ignore.case = TRUE))

  # check for versioned installation path
  # e.g. C:/RBuildTools/4.2
  version <- catch(numeric_version(name))
  if (!inherits(version, "error"))
    return(format(version))

  # detect older Rtools installations
  path <- file.path(root, "VERSION.txt")
  if (!file.exists(path))
    return(NULL)

  contents <- readLines(path, warn = FALSE)
  version <- gsub("[^[:digit:].]", "", contents)
  numeric_version(version)

}

renv_rtools_compatible <- function(spec) {

  if (is.null(spec$version))
    return(FALSE)

  ranges <- list(
    "4.5" = c("4.5.0", "9.9.9"),
    "4.4" = c("4.4.0", "9.9.9"),
    "4.3" = c("4.3.0", "4.4.0"),
    "4.2" = c("4.2.0", "4.3.0"),
    "4.0" = c("4.0.0", "4.2.0"),
    "3.5" = c("3.3.0", "4.0.0"),
    "3.4" = c("3.3.0", "4.0.0"),
    "3.3" = c("3.2.0", "3.3.0"),
    "3.2" = c("3.1.0", "3.2.0"),
    "3.1" = c("3.0.0", "3.1.0")
  )

  version <- numeric_version(spec$version)[1, 1:2]
  range <- ranges[[format(version)]]
  if (is.null(range))
    return(FALSE)

  rversion <- getRversion()
  range[[1]] <= rversion && rversion < range[[2]]

}

renv_rtools_registry <- function() {

  status <- tryCatch(
    utils::readRegistry(
      key = "SOFTWARE\\R-Core\\Rtools",
      hive = "HLM"
    ),
    error = function(e) list()
  )

  path <- status$InstallPath %||% ""
  if (file.exists(path))
    return(renv_path_normalize(path))

}

renv_rtools_envvars <- function(root) {

  version <- renv_rtools_version(root)

  if (version < "4.0")
    renv_rtools_envvars_default(root)
  else if (version < "4.2")
    renv_rtools_envvars_rtools40(root)
  else if (version < "4.3")
    renv_rtools_envvars_rtools42(root)
  else if (version < "4.4")
    renv_rtools_envvars_rtools43(root)
  else
    renv_rtools_envvars_default(root)

}

renv_rtools_envvars_default <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- ""

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools43 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- ""

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools42 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)

  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- ""

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools40 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF (note: trailing slash is required)
  binpref <- "/mingw$(WIN)/bin/"

  list(PATH = path, BINPREF = binpref)

}

# the Rtools series (e.g. "4.5") appropriate for a version of R. from R 4.2
# onwards, each minor release of R has a matching Rtools release
renv_rtools_required <- function(version = getRversion()) {

  version <- numeric_version(version)

  case(
    version >= "4.2.0" ~ renv_version_maj_min(version),
    version >= "4.0.0" ~ "4.0",
    ~ "3.5"
  )

}

# is a build toolchain available for use with this version of R?
renv_rtools_available <- function() {

  # an explicitly-requested or discovered Rtools installation
  root <- renv_paths_rtools()
  if (nzchar(root) && file.exists(root))
    return(TRUE)

  # otherwise, check for a toolchain the user has placed on the PATH
  make <- Sys.getenv("MAKE", unset = "make")
  nzchar(Sys.which(make)) && nzchar(Sys.which("gcc"))

}

# check whether any of the requested packages will need to be built from
# source on a Windows machine which lacks Rtools; the install would otherwise
# fail only after any packages which do have binaries have been installed
renv_rtools_preflight <- function(records) {

  if (!renv_platform_windows())
    return(TRUE)

  if (renv_rtools_available())
    return(TRUE)

  renv_rtools_preflight_impl(records)

}

renv_rtools_preflight_impl <- function(records, version = getRversion()) {

  records <- filter(records, renv_rtools_preflight_required)
  if (empty(records))
    return(TRUE)

  fmt <- "%s [%s]"
  values <- map_chr(records, function(record) {
    sprintf(fmt, record$Package, record$Version %||% "*")
  })

  preamble <- "The following package(s) are not available as binaries, and will be built from source:"

  fmt <- "Packages containing compiled code require Rtools %s, which was not found."
  postamble <- c(
    sprintf(fmt, renv_rtools_required(version)),
    "Install Rtools from https://cran.r-project.org/bin/windows/Rtools/, then restart R."
  )

  bulletin(preamble, sort(values), postamble)

  FALSE

}

# will installing this record require building it from source?
renv_rtools_preflight_required <- function(record) {

  # an already-built copy in the cache is reused as-is
  path <- renv_cache_find(record)
  if (nzchar(path) && file.exists(path))
    return(FALSE)

  package <- record$Package
  version <- record$Version
  source <- renv_record_source(record, normalize = TRUE)

  # Bioconductor repositories are only added to the repos option later
  # during the install, so we can't yet tell whether a binary is available
  if (source == "bioconductor")
    return(FALSE)

  # packages from the cellar may be available as binaries
  if (source %in% c("cellar", "local")) {
    path <- catch(renv_retrieve_cellar_find(record))
    if (!inherits(path, "error") && identical(names(path), "binary"))
      return(FALSE)
  }

  if (source == "repository") {

    # a binary is available from the active repositories
    types <- renv_graph_url_repository_types()
    if ("binary" %in% types) {

      entry <- catch(
        renv_available_packages_entry(
          package = package,
          type    = "binary",
          filter  = version
        )
      )

      if (!inherits(entry, "error"))
        return(FALSE)

    }

    # a binary is available from P3M
    if (renv_p3m_enabled() && !is.null(renv_retrieve_repos_p3m_date(record)))
      return(FALSE)

  }

  # a source build is needed; it only requires a toolchain if the package
  # contains compiled code. we can only read that from the current release
  # of the package in the source repositories, so assume compilation is
  # required when it isn't available
  entry <- catch(renv_available_packages_entry(package, type = "source"))
  if (!inherits(entry, "error") && identical(entry$NeedsCompilation, "no"))
    return(FALSE)

  TRUE

}
