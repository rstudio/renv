
# NOTE: all methods here should either return TRUE if they were able to
# operate successfully, or throw an error if not
renv_file_preface <- function(source, target, overwrite) {

  callback <- function() {}
  if (!file.exists(source))
    stopf("source file '%s' does not exist", source)

  if (overwrite)
    callback <- renv_file_scoped_backup(target)

  if (file.exists(target))
    stopf("target file '%s' already exists", target)

  callback

}

renv_file_copy <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  on.exit(callback(), add = TRUE)

  # check to see if we're copying a plain file -- if so, things are simpler
  info <- file.info(source)
  case(
    identical(info$isdir, FALSE) ~ renv_file_copy_file(source, target),
    identical(info$isdir, TRUE)  ~ renv_file_copy_dir(source, target)
  )

}

renv_file_copy_file <- function(source, target) {

  status <- catchall(file.copy(source, target))
  if (inherits(status, "condition"))
    stop(status)

  if (file.exists(target))
    return(TRUE)

  fmt <- "attempt to copy file '%s' to '%s' failed (unknown reason)"
  stopf(fmt, source, target)

}

renv_file_copy_dir <- function(source, target) {

  # first, copy into a unique sub-directory
  # then attempt to move back into the requested location
  tempfile <- tempfile("renv-copy-", tmpdir = dirname(target))
  ensure_directory(tempfile)
  on.exit(unlink(tempfile, recursive = TRUE), add = TRUE)

  status <- catchall(file.copy(source, tempfile, recursive = TRUE))
  if (inherits(status, "condition"))
    stop(status)

  temptarget <- file.path(tempfile, basename(source))
  status <- catchall(file.rename(temptarget, target))
  if (inherits(status, "condition"))
    stop(status)

  if (file.exists(target))
    return(TRUE)

  fmt <- "attempt to copy file '%s' to '%s' failed (unknown reason)"
  stopf(fmt, source, target)

}

renv_file_move <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  on.exit(callback(), add = TRUE)

  # now, attempt to rename (catchall since this might quietly fail for
  # cross-device links)
  move <- catchall(file.rename(source, target))
  if (file.exists(target))
    return(TRUE)

  copy <- catchall(renv_file_copy(source, target))
  if (identical(copy, TRUE))
    return(TRUE)

  # rename and copy both failed: inform the user
  fmt <- stack()
  fmt$push("could not copy / move file '%s' to '%s'")
  if (inherits(move, "condition"))
    fmt$push(paste("move:", conditionMessage(move)))
  if (inherits(copy, "condition"))
    fmt$push(paste("copy:", conditionMessage(copy)))

  stopf(fmt$data(), source, target)

}

renv_file_link <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  on.exit(callback(), add = TRUE)

  # first, try to symlink (note that this is supported on certain versions of
  # Windows as well when such permissions are enabled)
  status <- catchall(file.symlink(source, target))
  if (identical(status, TRUE) && file.exists(target))
    return(TRUE)

  # on Windows, try creating a junction point
  if (Sys.info()[["sysname"]] == "Windows") {
    status <- catchall(Sys.junction(source, target))
    if (identical(status, TRUE) && file.exists(target))
      return(TRUE)
  }

  # all else fails, just perform a copy
  info <- file.info(source)
  if (info$isdir) {
    ensure_parent_directory(target)
    file.copy(source, dirname(target), recursive = TRUE)
  } else {
    file.copy(source, target)
  }

  file.exists(target)

}

renv_file_same <- function(source, target) {

  # if the paths are the same, we can return early
  if (identical(source, target))
    return(TRUE)

  # check to see if they're equal after normalization
  source <- normalizePath(source, mustWork = FALSE)
  target <- normalizePath(target, mustWork = FALSE)
  if (identical(source, target))
    return(TRUE)

  # if either file doesn't exist, bail
  if (!file.exists(source) || !file.exists(target))
    return(FALSE)

  # otherwise, check and see if they're hardlinks to the same file
  test <- Sys.which("test")
  if (nzchar(test)) {
    command <- paste(test, shQuote(source), "-ef", shQuote(target))
    status <- catch(system(command))
    return(identical(status, 0L))
  }

  # try using fsutil (primarily for Windows + hardlinks)
  fsutil <- Sys.which("fsutil")
  if (nzchar(fsutil)) {
    prefix <- paste(shQuote(fsutil), "file queryFileID")
    sid    <- catch(system(paste(prefix, shQuote(source)), intern = TRUE))
    tid    <- catch(system(paste(prefix, shQuote(target)), intern = TRUE))
    return(identical(sid, tid))
  }

  # assume FALSE when all else fails
  FALSE

}

# NOTE: returns a callback which should be used in e.g. an on.exit handler
# to restore the file if the attempt to update the file failed
renv_file_scoped_backup <- function(path) {

  force(path)

  # if no file exists then nothing to backup
  if (!file.exists(path))
    return(function() {})

  # normalize the path (since the working directory could change
  # by the time the callback is invoked)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # attempt to rename the file
  pattern <- sprintf("renv-backup-%s", basename(path))
  tempfile <- tempfile(pattern, tmpdir = dirname(path))
  if (!file.rename(path, tempfile))
    return(function() {})

  # return callback that will restore if needed
  function() {

    if (!file.exists(path))
      file.rename(tempfile, path)
    else
      unlink(tempfile, recursive = TRUE)

  }

}

renv_file_alt <- function(path, alternate) {
  subpath <- file.path(path, alternate)
  if (file.exists(subpath)) subpath else path
}
