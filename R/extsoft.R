
renv_extsoft_curl_version <- function() {
  Sys.getenv("RENV_EXTSOFT_CURL_VERSION", unset = "7.77.0")
}

renv_extsoft_install <- function(quiet = FALSE) {

  extsoft <- renv_paths_extsoft()

  ensure_directory(extsoft)
  ensure_directory(file.path(extsoft, "lib/i386"))
  ensure_directory(file.path(extsoft, "lib/x64"))

  root <- "https://s3.amazonaws.com/rstudio-buildtools/extsoft"

  files <- c(
    sprintf("curl-%s-win32-mingw.zip", renv_extsoft_curl_version()),
    "glpk32.zip",
    "glpk64.zip",
    "local323.zip",
    "nlopt-2.4.2.zip",
    "spatial324.zip"
  )

  # check for missing installs
  files <- Filter(renv_extsoft_install_required, files)
  if (empty(files)) {
    if (!quiet) writef("- External software is up to date.")
    return(TRUE)
  }

  if (interactive()) {

    caution_bullets(
      "The following external software tools will be installed:",
      files,
      sprintf("Tools will be installed into %s.", renv_path_pretty(extsoft))
    )

    cancel_if(!proceed())

  }

  for (file in files) {

    # download the file
    url <- file.path(root, file)
    destfile <- renv_scope_tempfile("renv-archive-", fileext = ".zip")
    download(url, destfile = destfile, quiet = quiet)

    # write manifest
    manifest <- renv_extsoft_manifest_path(file)
    ensure_parent_directory(manifest)

    before <- list.files(extsoft, recursive = TRUE)

    # unpack archive
    if (file == "glpk32.zip") {

      unzip(destfile, files = "include/glpk.h", exdir = extsoft)
      unzip(destfile, exdir = file.path(extsoft, "lib/i386"), junkpaths = TRUE)

    } else if (file == "glpk64.zip") {

      unzip(destfile, files = "include/glpk.h", exdir = extsoft)
      unzip(destfile, exdir = file.path(extsoft, "lib/x64"), junkpaths = TRUE)

    } else if (file == "nlopt-2.4.2.zip") {

      unzip(destfile, exdir = extsoft)

      file.copy(file.path(extsoft, "nlopt-2.4.2/include"), extsoft, recursive = TRUE)
      file.copy(file.path(extsoft, "nlopt-2.4.2/lib"), extsoft, recursive = TRUE)
      unlink(file.path(extsoft, "nlopt-2.4.2"), recursive = TRUE)


    } else {

      unzip(destfile, exdir = extsoft)

    }

    after <- list.files(extsoft, recursive = TRUE)
    writeLines(setdiff(after, before), con = manifest)

  }

  writef("- External software successfully updated.")
  TRUE

}

renv_extsoft_install_required <- function(file) {

  manifest <- renv_extsoft_manifest_path(file)
  if (!file.exists(manifest))
    return(TRUE)

  files <- catch(readLines(manifest, warn = FALSE))
  if (inherits(files, "error"))
    return(FALSE)

  paths <- renv_paths_extsoft(files)
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
  contents <- insert(contents, "^#?LOCAL_SOFT", localsoft)

  localcpp <- "LOCAL_CPPFLAGS = -I\"$(LOCAL_SOFT)/include\""
  contents <- insert(contents, "^#?LOCAL_CPPFLAGS", localcpp)

  locallibs <- "LOCAL_LIBS = -L\"$(LOCAL_SOFT)/lib$(R_ARCH)\" -L\"$(LOCAL_SOFT)/lib\""
  contents <- insert(contents, "^#?LOCAL_LIBS", locallibs)

  libxml <- paste("LIB_XML", extsoft, sep = " = ")
  contents <- insert(contents, "^#?LIB_XML", libxml)

  if (identical(original, contents))
    return(TRUE)

  if (interactive()) {

    caution_bullets(
      "The following entries will be added to ~/.R/Makevars:",
      c(localsoft, libxml, localcpp, locallibs),
      "These tools will be used when compiling R packages from source."
    )

    cancel_if(!proceed())

  }

  if (!quiet) writef("- '%s' has been updated.", path)
  writeLines(contents, con = path)
  TRUE

}

renv_extsoft_manifest_path <- function(file) {
  name <- paste(file, "manifest", sep = ".")
  renv_paths_extsoft("manifests", name)
}
