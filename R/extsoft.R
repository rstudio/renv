
renv_extsoft_install <- function(quiet = FALSE) {

  extsoft <- renv_paths_extsoft()
  ensure_directory(extsoft)

  root <- "https://s3.amazonaws.com/rstudio-buildtools/extsoft"
  files <- c("local323.zip", "spatial324.zip")

  # check for missing installs
  files <- Filter(renv_extsoft_install_required, files)
  if (empty(files)) {
    if (!quiet) vwritef("* External software is up-to-date.")
    return(FALSE)
  }

  for (file in files) {

    # download the file
    url <- file.path(root, file)
    destfile <- tempfile(fileext = ".zip")
    download(url, destfile = destfile, quiet = quiet)

    # write manifest
    manifest <- renv_paths_extsoft("manifest", file, "manifest.rds")
    ensure_parent_directory(manifest)
    listed <- unzip(destfile, list = TRUE)
    saveRDS(listed, file = manifest)

    # unpack archive
    unzip(destfile, exdir = extsoft)

  }

}

renv_extsoft_install_required <- function(file) {

  manifest <- renv_paths_extsoft("manifest", file, "manifest.rds")
  if (!file.exists(manifest))
    return(TRUE)

  db <- catch(readRDS(manifest))
  if (inherits(db, "error"))
    return(TRUE)

  # TODO: could also validate file sizes
  names <- db$Name
  paths <- renv_paths_extsoft(names)
  !all(file.exists(paths))

}

# TODO: is there something more appropriate than modifying the installed Makeconf?
renv_extsoft_use <- function(quiet = FALSE) {

  extsoft <- renv_paths_extsoft()
  paths <- file.path(R.home("etc"), c("i386", "x64"), "Makeconf")

  status <- file.access(paths, 2)
  if (!all(status == 0)) {

    lines <- c(
      paste("LOCAL_SOFT", extsoft, sep = " = "),
      paste("LIB_XML", extsoft, sep = " = ")
    )

    renv_pretty_print_packages(
      lines,
      "Please add the following lines to your Makeconf files to complete installation:",
      wrap = FALSE
    )

    renv_pretty_print_packages(
      normalizePath(paths, winslash = "/"),
      "R Makeconf files are located at:",
      wrap = FALSE
    )

    return(FALSE)

  }

  for (path in paths) {

    # some installations may be 32-bit or 64-bit only; in that case we
    # may not see the associated Makeconf file
    if (!file.exists(path))
      next

    original <- contents <- readLines(path)

    localsoft <- paste("LOCAL_SOFT", extsoft, sep = " = ")
    contents <- inject(contents, "^#?LOCAL_SOFT", localsoft, "^#?NM_FILTER")

    libxml <- paste("LIB_XML", extsoft, sep = " = ")
    contents <- inject(contents, "^#?LIB_XML", libxml, "^#?LOCAL_SOFT")

    if (!identical(original, contents)) {
      if (!quiet) vwritef("* Updating '%s'.", path)
      writeLines(contents, path)
    }

  }

  invisible(TRUE)

}

