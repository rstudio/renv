
renv_extsoft_install <- function() {

  extsoft <- renv_paths_extsoft()
  ensure_directory(extsoft)

  # TODO: how to detect if we've already installed these successfully?
  root <- "https://s3.amazonaws.com/rstudio-buildtools/extsoft"
  for (file in c("local323.zip", "spatial324.zip")) {
    url <- file.path(root, file)
    destfile <- tempfile(fileext = ".zip")
    download(url, destfile = destfile)
    unzip(destfile, exdir = extsoft)
  }

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

    contents <- readLines(path)

    localsoft <- paste("LOCAL_SOFT", extsoft, sep = " = ")
    contents <- inject(contents, "^#?LOCAL_SOFT", localsoft, "^#?NM_FILTER")

    libxml <- paste("LIB_XML", extsoft, sep = " = ")
    contents <- inject(contents, "^#?LIB_XML", libxml, "^#?LOCAL_SOFT")

    writeLines(contents, path)

  }

  invisible(TRUE)

}
