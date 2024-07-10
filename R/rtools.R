
renv_rtools_list <- function() {

  drive <- Sys.getenv("SYSTEMDRIVE", unset = "C:")

  roots <- c(

    renv_rtools_registry(),

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
