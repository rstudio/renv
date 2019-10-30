
renv_rtools_find <- function() {

  drive <- Sys.getenv("SYSTEMDRIVE", unset = "C:")

  roots <- c(

    Sys.getenv("RENV_PATH_RTOOLS"),
    renv_rtools_registry(),

    file.path(drive, "rtools40"),
    file.path(drive, "Rtools"),
    list.files(file.path(drive, "RBuildTools"), full.names = TRUE),

    "~/Rtools",
    list.files("~/RBuildTools", full.names = TRUE)

  )

  roots <- unique(roots[file.exists(roots)])
  specs <- lapply(roots, renv_rtools_read)
  compatible <- Filter(renv_rtools_compatible, specs)

  if (length(compatible) == 0)
    return(NULL)

  compatible[[1]]$root

}

renv_rtools_read <- function(root) {

  path <- file.path(root, "VERSION.txt")
  if (!file.exists(path))
    return(NULL)

  contents <- readLines(path, warn = FALSE)
  version <- gsub("[^[:digit:].]", "", contents)

  list(
    root = root,
    version = numeric_version(version)
  )

}

renv_rtools_compatible <- function(spec) {

  ranges <- list(
    "4.0" = c("4.0.0", "9.9.9"),
    "3.5" = c("3.3.0", "4.0.0"),
    "3.4" = c("3.3.0", "4.0.0"),
    "3.3" = c("3.2.0", "3.3.0"),
    "3.2" = c("3.1.0", "3.2.0"),
    "3.1" = c("3.0.0", "3.1.0")
  )

  version <- spec$version[1, 1:2]
  range <- ranges[[format(version)]]
  if (is.null(range))
    return(FALSE)

  rversion <- getRversion()
  range[[1]] < rversion && rversion < range[[2]]

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
