
renv_extsoft_install <- function(quiet = FALSE) {

  extsoft <- renv_paths_extsoft()
  ensure_directory(extsoft)

  root <- "https://s3.amazonaws.com/rstudio-buildtools/extsoft"
  files <- c("local323.zip", "spatial324.zip", "curl-7.64.1_1-win32-mingw.zip")

  # check for missing installs
  files <- Filter(renv_extsoft_install_required, files)
  if (empty(files)) {
    if (!quiet) vwritef("* External software is up to date.")
    return(TRUE)
  }

  if (interactive()) {

    renv_pretty_print(
      files,
      "The following external software tools will be installed:",
      sprintf("Tools will be installed into '%s'.", aliased_path(extsoft)),
      wrap = FALSE
    )

    if (!proceed()) {
      message("* Operation aborted.")
      return(FALSE)
    }

  }

  for (file in files) {

    # download the file
    url <- file.path(root, file)
    destfile <- renv_tempfile("renv-archive-", fileext = ".zip")
    download(url, destfile = destfile, quiet = quiet)

    # write manifest
    manifest <- renv_extsoft_manifest_path(file)
    ensure_parent_directory(manifest)
    listed <- unzip(destfile, list = TRUE)
    saveRDS(listed, file = manifest)

    # unpack archive
    unzip(destfile, exdir = extsoft)

  }

  vwritef("* External software successfully updated.")
  TRUE

}

renv_extsoft_install_required <- function(file) {

  manifest <- renv_extsoft_manifest_path(file)
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

renv_extsoft_use <- function(quiet = FALSE) {

  extsoft <- renv_paths_extsoft()
  path <- "~/.R/Makevars"

  ensure_parent_directory(path)
  original <- if (file.exists(path))
    readLines(path, warn = FALSE)
  else
    character()

  contents <- original

  localsoft <- paste("LOCAL_SOFT", extsoft, sep = " = ")
  contents <- inject(contents, "^#?LOCAL_SOFT", localsoft)

  localcpp <- "LOCAL_CPPFLAGS = -I\"$(LOCAL_SOFT)/include\""
  contents <- inject(contents, "^#?LOCAL_CPPFLAGS", localcpp)

  locallibs <- "LOCAL_LIBS = -L\"$(LOCAL_SOFT)/lib$(R_ARCH)\" -L\"$(LOCAL_SOFT)/lib\""
  contents <- inject(contents, "^#?LOCAL_LIBS", locallibs)

  libxml <- paste("LIB_XML", extsoft, sep = " = ")
  contents <- inject(contents, "^#?LIB_XML", libxml)

  if (identical(original, contents))
    return(TRUE)

  if (interactive()) {

    renv_pretty_print(
      c(localsoft, libxml, localcpp, locallibs),
      "The following entries will be added to ~/.R/Makevars:",
      "These tools will be used when compiling R packages from source.",
      wrap = FALSE
    )

    if (!proceed()) {
      message("* Operation aborted.")
      return(FALSE)
    }

  }

  if (!quiet) vwritef("* '%s' has been updated.", path)
  writeLines(contents, path)
  TRUE

}

renv_extsoft_manifest_path <- function(file) {
  name <- paste(file, "manifest", sep = ".")
  renv_paths_extsoft("manifest", name)
}
