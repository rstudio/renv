
renv_rtools_list <- function() {

  drive <- Sys.getenv("SYSTEMDRIVE", unset = "C:")

  roots <- c(

    renv_rtools_registry(),

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

  # detect Rtools40
  mirrors <- file.path(root, "etc/pacman.d/mirrorlist.rtools")
  if (file.exists(mirrors))
    return(numeric_version("4.0"))

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
    "4.0" = c("4.0.0", "9.9.9"),
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
    return(normalizePath(path, winslash = "/"))

}

renv_rtools_envvars <- function(root) {

  version <- renv_rtools_version(root)

  if (version < "4.0")
    renv_rtools_envvars_default(root)
  else
    renv_rtools_envvars_rtools40(root)

}

renv_rtools_envvars_default <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(
    file.path(root, "bin"),
    winslash = "\\",
    mustWork = FALSE
  )

  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF (note: trailing slash required but file.path()
  # drops trailing slashes on Windows)
  binpref <- paste(
    normalizePath(root, winslash = "/", mustWork = FALSE),
    "mingw_$(WIN)/bin/",
    sep = "/"
  )

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools40 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(
    file.path(root, "usr/bin"),
    winslash = "\\",
    mustWork = FALSE
  )

  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- "/mingw$(WIN)/bin/"

  list(PATH = path, BINPREF = binpref)

}
