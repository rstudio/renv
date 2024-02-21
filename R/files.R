
# NOTE: all methods here should either return TRUE if they were able to
# operate successfully, or throw an error if not
#
# TODO: some of these operations are a bit racy
renv_file_preface <- function(source, target, overwrite) {

  callback <- function() {}
  if (!renv_file_exists(source))
    stopf("source file '%s' does not exist", source)

  if (overwrite)
    callback <- renv_file_backup(target)

  if (renv_file_exists(target))
    stopf("target file '%s' already exists", target)

  callback

}

renv_file_copy <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  defer(callback())

  # check to see if we're copying a plain file -- if so, things are simpler
  if (dir.exists(source))
    renv_file_copy_dir(source, target)
  else
    renv_file_copy_file(source, target)

}

renv_file_copy_file <- function(source, target) {

  # copy to temporary path
  tmpfile <- renv_scope_tempfile(".renv-copy-", tmpdir = dirname(target))
  status <- catchall(file.copy(source, tmpfile))
  if (inherits(status, "condition"))
    stop(status)

  # move from temporary path to final target
  status <- catchall(renv_file_move(tmpfile, target))
  if (inherits(status, "condition"))
    stop(status)

  # validate that the target file exists
  if (!renv_file_exists(target)) {
    fmt <- "attempt to copy file %s to %s failed (unknown reason)"
    stopf(fmt, renv_path_pretty(source), renv_path_pretty(target))
  }

  invisible(TRUE)

}

renv_file_copy_dir_robocopy <- function(source, target) {
  renv_robocopy_copy(source, target)
}

# TODO: the version of rsync distributed with macOS
# does not reliably copy file modified times, etc.
renv_file_copy_dir_rsync <- function(source, target) {
  source <- sub("/*$", "/", source)
  flags <- if (renv_platform_macos()) "-aAX" else "-a"
  args <- c(flags, renv_shell_path(source), renv_shell_path(target))
  renv_system_exec("rsync", args, action = "copying directory")
}

renv_file_copy_dir_cp <- function(source, target) {

  # ensure 'source' ends with a single trailing slash
  source <- sub("/*$", "/", source)

  # ensure tildes are path-expanded
  source <- path.expand(source)
  target <- path.expand(target)

  # build 'cp' arguments
  args <- c("-PR", renv_shell_path(source), renv_shell_path(target))

  # execute command
  renv_system_exec("cp", args, action = "copying directory")

}

renv_file_copy_dir_r <- function(source, target) {

  # create sub-directory to host copy attempt
  tempdir <- renv_scope_tempfile(".renv-copy-", tmpdir = dirname(target))
  ensure_directory(tempdir)

  # attempt to copy to generated folder
  status <- catchall(
    file.copy(
      source,
      tempdir,
      recursive = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  )

  if (inherits(status, "error"))
    stop(status)

  # R will copy the directory to a sub-directory in the
  # requested folder with the same filename as the source
  # folder, so peek into that folder to grab it and rename
  tempfile <- file.path(tempdir, basename(source))
  status <- catchall(renv_file_move(tempfile, target))
  if (inherits(status, "condition"))
    stop(status)

}

renv_file_copy_dir_impl <- function(source, target) {

  methods <- list(
    cp       = renv_file_copy_dir_cp,
    r        = renv_file_copy_dir_r,
    robocopy = renv_file_copy_dir_robocopy,
    rsync    = renv_file_copy_dir_rsync
  )

  copy <- config$copy.method()
  if (is.function(copy))
    return(copy(source, target))

  method <- methods[[tolower(copy)]]
  if (!is.null(method))
    return(method(source, target))

  if (renv_platform_windows())
    renv_file_copy_dir_robocopy(source, target)
  else if (renv_platform_unix())
    renv_file_copy_dir_cp(source, target)
  else
    renv_file_copy_dir_r(source, target)

  file.exists(target)

}

renv_file_copy_dir <- function(source, target) {

  # create temporary sub-directory
  tmpdir <- dirname(target)
  ensure_directory(tmpdir)
  tempdir <- renv_scope_tempfile(".renv-copy-", tmpdir = tmpdir)

  # copy to that directory
  status <- catchall(renv_file_copy_dir_impl(source, tempdir))
  if (inherits(status, "condition"))
    stop(status)

  # move directory to final location
  status <- catchall(renv_file_move(tempdir, target))
  if (inherits(status, "condition"))
    stop(status)

  # validate that the target file exists
  if (!renv_file_exists(target)) {
    fmt <- "attempt to copy directory %s to %s failed (unknown reason)"
    stopf(fmt, renv_path_pretty(source), renv_path_pretty(target))
  }

  invisible(TRUE)

}

renv_file_move <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  defer(callback())

  # first, attempt to do a plain rename
  # use catchall since this might fail for e.g. cross-device links
  # (note that junction points on Windows will be copies as-is)
  move <- catchall(file.rename(source, target))
  if (renv_file_exists(target))
    return(TRUE)

  # expand tildes
  source <- path.expand(source)
  target <- path.expand(target)

  # on unix, try using 'mv' command directly
  # (can handle cross-device copies / moves a bit more efficiently)
  if (renv_platform_unix()) {
    args <- c(renv_shell_path(source), renv_shell_path(target))
    status <- catchall(system2("mv", args, stdout = FALSE, stderr = FALSE))
    if (renv_file_exists(target))
      return(TRUE)
  }

  # on Windows, similarly try 'robocopy' command
  # (should be faster than 'move' for large directories)
  if (renv_platform_windows()) {
    status <- catchall(renv_robocopy_move(source, target))
    if (renv_file_exists(target))
      return(TRUE)
  }

  # nocov start
  # rename failed; fall back to copying
  # (and be sure to remove the source file / directory on success)
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

  text <- paste(fmt$data(), collapse = "\n")
  stopf(text, source, target)
  # nocov end

}

renv_file_link <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  defer(callback())

  if (renv_platform_windows()) {

    # use junction points on Windows by default as symlinks
    # are unreliable / un-deletable in some circumstances
    status <- catchall(Sys.junction(source, target))
    if (identical(status, TRUE))
      return(TRUE)

    # if Sys.junction() fails, it may leave behind an empty
    # directory. this may occur if the source and target files
    # reside on different volumes. either way, remove an empty
    # left-behind directory on failure
    unlink(target, recursive = TRUE, force = TRUE)

  } else {

    # on non-Windows, we can try to create a symlink
    status <- catchall(file.symlink(source, target))
    if (identical(status, TRUE))
      return(TRUE)

  }

  # all else fails, just perform a copy
  renv_file_copy(source, target, overwrite = overwrite)

}

renv_file_junction <- function(source, target) {

  if (!renv_platform_windows())
    stopf("'renv_file_junction()' is only available on Windows")

  if (renv_file_exists(target))
    stopf("file '%s' already exists")

  status <- catchall(Sys.junction(source, target))
  if (inherits(status, "condition")) {
    unlink(target, recursive = TRUE, force = TRUE)
    stop(status)
  }

  TRUE

}

renv_file_same <- function(source, target) {

  # if the paths are the same, we can return early
  if (identical(source, target))
    return(TRUE)

  # check to see if they're equal after normalization
  # (e.g. for symlinks pointing to same file)
  source <- renv_path_normalize(source)
  target <- renv_path_normalize(target)
  if (identical(source, target))
    return(TRUE)


  # for hard links + junction points, it's difficult to detect
  # whether the two files point to the same object; use some
  # heuristics to guess (note that these aren't perfect)
  sinfo <- renv_file_info(source)
  if (is.na(sinfo$isdir))
    return(FALSE)

  tinfo <- renv_file_info(target)
  if (is.na(tinfo$isdir))
    return(FALSE)

  # NOTE: we intentionally exclude 'size' for directories, as
  # on Windows the 'size' field might be reported as 0 for
  # junction points?
  if (renv_platform_windows()) {
    if (sinfo$isdir || tinfo$isdir)
      sinfo$size <- tinfo$size <- 0L
  }

  identical(c(sinfo), c(tinfo))

}

# NOTE: returns a callback which should be used in e.g. an defer handler
# to restore the file if the attempt to update the file failed
renv_file_backup <- function(path) {

  # if no file exists then nothing to backup
  if (!renv_file_exists(path))
    return(function() {})

  # normalize the path (since the working directory could change
  # by the time the callback is invoked). note that the file may
  # be a broken symlink so construct the path by normalizing the
  # parent directory and building path relative to that
  parent <- renv_path_normalize(dirname(path), mustWork = TRUE)
  path <- file.path(parent, basename(path))

  # attempt to rename the file
  pattern <- sprintf(".renv-backup-%i-%s", Sys.getpid(), basename(path))
  tempfile <- tempfile(pattern, tmpdir = dirname(path))
  if (!renv_file_move(path, tempfile))
    stopf("couldn't rename '%s' prior to transaction", renv_path_pretty(path))

  # return callback that will restore if needed
  function() {

    if (!renv_file_exists(path))
      renv_file_move(tempfile, path)
    else
      unlink(tempfile, recursive = TRUE)

  }

}

renv_file_info <- function(paths, extra_cols = FALSE) {
  suppressWarnings(file.info(paths, extra_cols = extra_cols))
}

renv_file_mode <- function(paths) {
  suppressWarnings(file.mode(paths))
}

# NOTE: returns true for files that are broken symlinks
renv_file_exists <- function(path) {

  if (renv_platform_windows())
    renv_file_exists_win32(path)
  else
    renv_file_exists_unix(path)

}

renv_file_exists_win32 <- function(path) {
  file.exists(path)
}

renv_file_exists_unix <- function(path) {
  !is.na(Sys.readlink(path)) | file.exists(path)
}

renv_file_list <- function(path, full.names = TRUE) {

  # list files
  files <- renv_file_list_impl(path)

  # NOTE: paths may be marked with UTF-8 encoding;
  # if that's the case we need to use paste rather
  # than file.path to preserve the encoding
  if (full.names && length(files))
    files <- paste(path, files, sep = "/")

  files

}

renv_file_list_impl <- function(path) {
  if (renv_platform_unix())
    renv_file_list_impl_unix(path)
  else
    renv_file_list_impl_win32(path)
}

renv_file_list_impl_unix <- function(path) {
  list.files(path, all.files = TRUE, no.. = TRUE)
}

# nocov start
renv_file_list_impl_win32 <- function(path) {

  # first, try a plain list.files to see if we can get away with that
  files <- list.files(path, all.files = TRUE, no.. = TRUE)
  if (!any(grepl("?", files, fixed = TRUE)))
    return(files)

  # otherwise, try some madness ...
  #
  # change working directory (done just to avoid encoding issues
  # when submitting path to cmd shell)
  renv_scope_wd(path)

  # NOTE: a sub-shell is required here in some contexts; e.g. when running
  # tests non-interactively or building in the RStudio pane
  command <- paste(comspec(), "/U /C dir /B")
  conn <- pipe(command, open = "rb", encoding = "native.enc")
  defer(close(conn))

  # read binary output from connection
  output <- stack()

  while (TRUE) {

    data <- readBin(conn, what = "raw", n = 1024L)
    if (empty(data))
      break

    output$push(data)

  }

  # join into single raw vector
  encoded <- unlist(output$data(), recursive = FALSE, use.names = FALSE)

  # convert raw data (encoded as UTF-16LE) to UTF-8
  converted <- iconv(list(encoded), from = "UTF-16LE", to = "UTF-8")

  # split on (Windows) newlines
  paths <- strsplit(converted, "\r\n", fixed = TRUE)[[1]]

  # just in case?
  paths[nzchar(paths)]

}
# nocov end

renv_file_type <- function(paths, symlinks = TRUE) {

  info <- renv_file_info(paths)

  types <- character(length(paths))
  types[info$isdir %in% FALSE] <- "file"
  types[info$isdir %in% TRUE ] <- "directory"

  if (symlinks && !renv_platform_windows()) {
    links <- Sys.readlink(paths)
    types[!is.na(links) & nzchar(links)] <- "symlink"
  }

  types

}

# nocov start
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
# nocov end

renv_file_find <- function(path, predicate) {

  # canonicalize path
  # (note: don't normalize as we don't want to follow symlinks)
  path <- renv_path_canonicalize(path)
  parent <- dirname(path)

  # compute number of slashes
  # (avoid searching beyond home directory, unless we're virtualized)
  virtualized <- renv_virtualization_type() != "native"
  slashes <- gregexpr("/", path, fixed = TRUE)[[1L]]
  n <- length(slashes) - if (virtualized) 0L else 2L

  for (i in 1:n) {

    if (file.exists(path)) {
      status <- predicate(path)
      if (!is.null(status))
        return(status)
    }

    path <- parent
    parent <- dirname(path)

  }

  predicate(path)

}

renv_file_read <- function(path) {
  renv_scope_options(warn = -1L)
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")
  paste(contents, collapse = "\n")
}

renv_file_shebang <- function(path) {

  # NOTE: we use 'condition' as a cheap way to capture both errors and warnings
  # since 'file()' may just report a warning rather than an error if it fails
  # to open a file due to inadequate permissions
  tryCatch(
    renv_file_shebang_impl(path),
    condition = function(e) ""
  )

}

renv_file_shebang_impl <- function(path) {

  renv_scope_options(warn = -1L)

  # open connection to file
  con <- file(path, open = "rb", encoding = "native.enc")
  defer(close(con))

  # validate file starts with '#!' -- read using 'raw' vector to avoid
  # issues which files that might start with null bytes
  bytes <- readBin(con, what = "raw", n = 2L)
  expected <- as.raw(c(0x23L, 0x21L))
  if (!identical(bytes, expected))
    return("")

  # read a single line from the connection
  readLines(con, n = 1L, warn = FALSE)

}

# here, 'broken' implies a file which is a link pointing to a file that
# doesn't exist, so only returns true if the file is "link"-y and the
# file it points to doesn't exist
renv_file_broken <- function(paths) {
  if (renv_platform_unix())
    renv_file_broken_unix(paths)
  else
    renv_file_broken_win32(paths)
}

renv_file_broken_unix <- function(paths) {
  # a symlink is broken if:
  # - the file is a symlink (tested via Sys.readlink)
  # - the file it points to does not exist (tested via file.exists)
  !is.na(Sys.readlink(paths)) & !file.exists(paths)
}

renv_file_broken_win32 <- function(paths) {
  # TODO: the behavior of file.exists() for a broken junction point
  # appears to have changed in the development version of R;
  # we have to be extra careful here...
  if (getRversion() < "4.2.0") {
    info <- renv_file_info(paths)
    (info$isdir %in% TRUE) & is.na(info$mtime)
  } else {
    file.access(paths, mode = 0L) == 0L & !file.exists(paths)
  }
}

renv_file_size <- function(path) {
  file.info(path, extra_cols = FALSE)$size
}

renv_file_remove <- function(paths) {
  if (renv_platform_windows())
    renv_file_remove_win32(paths)
  else
    renv_file_remove_unix(paths)
}

renv_file_remove_win32 <- function(paths) {
  for (path in paths) {
    command <- paste("rmdir /S /Q", renv_shell_path(path))
    shell(command)
  }
}

renv_file_remove_unix <- function(paths) {
  unlink(paths, recursive = TRUE, force = TRUE)
}

renv_file_writable <- function(path) {

  # allow users to opt-out just in case
  override <- getOption("renv.download.check_writable", default = TRUE)
  if (!identical(override, TRUE))
    return(TRUE)

  # if we're given the path to a file, use the parent directory instead
  info <- renv_file_info(path)
  if (!identical(info$isdir, TRUE))
    path <- dirname(path)

  # if we still don't have a directory, bail
  info <- renv_file_info(path)
  if (!identical(info$isdir, TRUE))
    return(FALSE)

  # try creating and removing a temporary file in this directory
  tempfile <- renv_scope_tempfile(".renv-write-test-", tmpdir = path)
  ok <- dir.create(tempfile, showWarnings = FALSE)

  # return ok if we succeeded
  ok

}
