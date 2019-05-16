
# NOTE: all methods here should either return TRUE if they were able to
# operate successfully, or throw an error if not
#
# TODO: some of these operations are a bit racy
renv_file_preface <- function(source, target, overwrite) {

  callback <- function() {}
  if (!file.exists(source))
    stopf("source file '%s' does not exist", source)

  if (overwrite)
    callback <- renv_file_scoped_backup(target)

  if (renv_file_exists(target))
    stopf("target file '%s' already exists", target)

  callback

}

renv_file_copy <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  on.exit(callback(), add = TRUE)

  # check to see if we're copying a plain file -- if so, things are simpler
  info <- file.info(source, extra_cols = FALSE)
  case(
    identical(info$isdir, FALSE) ~ renv_file_copy_file(source, target),
    identical(info$isdir, TRUE)  ~ renv_file_copy_dir(source, target)
  )

}

renv_file_copy_file <- function(source, target) {

  # copy to temporary path
  temptarget <- renv_tempfile("renv-copy-", tmpdir = dirname(target))
  status <- catchall(file.copy(source, temptarget))
  if (inherits(status, "condition"))
    stop(status)

  # move from temporary path to final target
  status <- catchall(file.rename(temptarget, target))
  if (inherits(status, "condition"))
    stop(status)

  # validate that the target file exists
  if (renv_file_exists(target))
    return(TRUE)

  fmt <- "attempt to copy file '%s' to '%s' failed (unknown reason)"
  stopf(fmt, source, target)

}

renv_file_copy_dir <- function(source, target) {

  # copy to temporary sub-directory
  tempdir <- renv_tempfile("renv-copy-", tmpdir = dirname(target))
  ensure_directory(tempdir)

  status <- catchall(file.copy(source, tempdir, recursive = TRUE))
  if (inherits(status, "condition"))
    stop(status)

  # R will copy the directory to a sub-directory in the
  # requested folder with the same filename as the source
  # folder, so peek into that folder to grab it and rename
  tempfile <- file.path(tempdir, basename(source))
  status <- catchall(file.rename(tempfile, target))
  if (inherits(status, "condition"))
    stop(status)

  # validate target now exists
  if (renv_file_exists(target))
    return(TRUE)

  fmt <- "attempt to copy directory '%s' to '%s' failed (unknown reason)"
  stopf(fmt, source, target)

}

renv_file_move <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  on.exit(callback(), add = TRUE)

  # first, attempt to do a plain rename
  # use catchall since this might fail for e.g. cross-device links
  move <- catchall(file.rename(source, target))
  if (renv_file_exists(target))
    return(TRUE)

  # rename failed; fall back to copying (and be sure to remove
  # the source file / directory on success)
  copy <- catchall(renv_file_copy(source, target, overwrite = overwrite))
  if (identical(copy, TRUE) && file.exists(target)) {
    unlink(source, recursive = TRUE)
    return(TRUE)
  }

  # rename and copy both failed: inform the user
  fmt <- stack()
  fmt$push("could not copy / move file '%s' to '%s'")
  if (inherits(move, "condition"))
    fmt$push(paste("move:", conditionMessage(move)))
  if (inherits(copy, "condition"))
    fmt$push(paste("copy:", conditionMessage(copy)))

  stopf(fmt$data(), source, target)

}

renv_file_link <- function(source, target, overwrite = FALSE, link = NULL) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  on.exit(callback(), add = TRUE)

  # use junction points on Windows by default as symlinks
  # are unreliable / un-deletable in some circumstances
  link <- link %||% if (renv_platform_windows())
    Sys.junction
  else
    file.symlink

  status <- catchall(link(source, target))
  if (identical(status, TRUE) && renv_file_exists(target))
    return(TRUE)

  # all else fails, just perform a copy
  renv_file_copy(source, target, overwrite = overwrite)

}

renv_file_same <- function(source, target) {

  # if the paths are the same, we can return early
  if (identical(source, target))
    return(TRUE)

  # check to see if they're equal after normalization
  # (e.g. for symlinks pointing to same file)
  source <- normalizePath(source, mustWork = FALSE)
  target <- normalizePath(target, mustWork = FALSE)
  if (identical(source, target))
    return(TRUE)

  # if either file is missing, return false
  if (!renv_file_exists(source) || !renv_file_exists(target))
    return(FALSE)

  # for hard links + junction points, it's difficult to detect
  # whether the two files point to the same object; use some
  # heuristics to guess (note that these aren't perfect)
  sinfo <- file.info(source, extra_cols = FALSE)
  tinfo <- file.info(target, extra_cols = FALSE)
  if (!identical(c(sinfo), c(tinfo)))
    return(FALSE)

  TRUE

}

# NOTE: returns a callback which should be used in e.g. an on.exit handler
# to restore the file if the attempt to update the file failed
renv_file_scoped_backup <- function(path) {

  force(path)

  # if no file exists then nothing to backup
  if (!renv_file_exists(path))
    return(function() {})

  # normalize the path (since the working directory could change
  # by the time the callback is invoked). note that the file may
  # be a broken symlink so construct the path by normalizing the
  # parent directory and building path relative to that
  parent <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  path <- file.path(parent, basename(path))

  # attempt to rename the file
  pattern <- sprintf("renv-backup-%s", basename(path))
  tempfile <- tempfile(pattern, tmpdir = dirname(path))
  if (!file.rename(path, tempfile))
    return(function() {})

  # return callback that will restore if needed
  function() {

    if (!renv_file_exists(path))
      file.rename(tempfile, path)
    else
      unlink(tempfile, recursive = TRUE)

  }

}

renv_file_normalize <- function(path, winslash = "\\", mustWork = NA) {
  parent <- normalizePath(dirname(path), winslash = winslash, mustWork = mustWork)
  file.path(parent, basename(path))
}

# NOTE: returns true for files that are broken symlinks
renv_file_exists <- function(path) {

  if (renv_platform_windows())
    return(file.exists(path))

  !is.na(Sys.readlink(path)) | file.exists(path)

}

renv_file_list <- function(path, full.names = TRUE) {
  files <- renv_file_list_impl(path)
  if (full.names) file.path(path, files) else files
}

renv_file_list_impl <- function(path) {

  # on Windows, list.files mangles encoding; avoid this by making a call to
  # 'dir' with the code page set to request UTF-8 encoded paths
  if (renv_platform_windows()) {
    path <- normalizePath(path)
    command <- paste(comspec(), "/c chcp 65001 && dir /B", shQuote(path))
    output <- system(command, intern = TRUE)
    Encoding(output) <- "UTF-8"
    return(output)
  }

  # otherwise, a plain old list.files will suffice
  list.files(path)

}

renv_file_type <- function(paths, symlinks = TRUE) {

  info <- file.info(paths, extra_cols = FALSE)

  types <- character(length(paths))
  types[info$isdir %in% FALSE] <- "file"
  types[info$isdir %in% TRUE ] <- "directory"

  if (symlinks && !renv_platform_windows()) {
    links <- Sys.readlink(paths)
    types[!is.na(links) & nzchar(links)] <- "symlink"
  }

  types

}

renv_file_compressor <- function(path) {

  ext <- fileext(path)
  if (ext == ".zip")
    return(zip)
  tar

}

renv_file_decompressor <- function(path) {

  ext <- fileext(path)
  if (ext == ".zip")
    return(unzip)
  untar

}

renv_file_edit <- function(path) {

  # https://github.com/rstudio/renv/issues/44
  dlls <- getLoadedDLLs()
  if (is.null(dlls[["(embedding)"]]))
    return(utils::file.edit(path))

  routines <- getDLLRegisteredRoutines("(embedding)")
  routine <- routines[[".Call"]][["rs_editFile"]]
  if (is.null(routine))
    return(utils::file.edit(path))

  do.call(.Call, list(routine, path, PACKAGE = "(embedding)"))

}

renv_file_find <- function(path, predicate, limit = 3) {

  parent <- dirname(path)
  while (path != parent && limit > 0) {

    if (file.exists(path)) {
      status <- predicate(path)
      if (!is.null(status))
        return(status)
    }

    path <- parent; parent <- dirname(path)
    limit <- limit - 1

  }

  predicate(path)

}
