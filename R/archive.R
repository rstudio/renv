# detect the archive type from the file's contents, falling back to the
# file extension when the path is not a readable file (e.g. a directory,
# or a file which hasn't been downloaded yet) or the contents aren't
# recognized. repositories don't always name binaries by their contents:
# a ZIP binary may be served as '.tar.xz' and vice versa, as with Windows
# custom binary types (#2422), and new extensions appear as repositories
# adopt new compressions. R-devel (r90464) sniffs its custom binaries the
# same way.
#
# https://github.com/rstudio/renv/issues/2424
renv_archive_type <- function(archive) {

  type <- renv_archive_type_sniff(archive)
  if (!is.null(type))
    return(type)

  ext <- fileext(archive)

  # both R's internal tar and system tar detect the compression used
  # (gzip, bzip2, xz, zstd, or none) from the archive contents, so any
  # tarball can be handled uniformly here
  if (ext %in% c(".tar", ".tgz") || startsWith(ext, ".tar."))
    return("tar")
  else if (ext %in% c(".zip"))
    return("zip")
  else
    return("unknown")

}

renv_archive_type_sniff <- function(archive) {

  info <- renv_file_info(archive)
  if (!identical(info$isdir, FALSE))
    return(NULL)

  # 'ustar' sits at offset 257 in an uncompressed tar header
  bytes <- catch(readBin(archive, what = "raw", n = 262L))
  if (!is.raw(bytes))
    return(NULL)

  # any ZIP archive starts with 'PK'; the next two bytes differ between
  # local file headers (03 04), empty archives (05 06), and spanned
  # archives (07 08)
  if (renv_raw_starts_with(bytes, c(0x50, 0x4b)))
    return("zip")

  # compressed tarballs; the tar tools detect the compression themselves
  magics <- list(
    gzip  = c(0x1f, 0x8b),
    bzip2 = c(0x42, 0x5a, 0x68),
    xz    = c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00),
    zstd  = c(0x28, 0xb5, 0x2f, 0xfd)
  )

  for (magic in magics)
    if (renv_raw_starts_with(bytes, magic))
      return("tar")

  # uncompressed tar
  if (length(bytes) >= 262L && identical(bytes[258:262], charToRaw("ustar")))
    return("tar")

  NULL

}

renv_raw_starts_with <- function(bytes, prefix) {
  n <- length(prefix)
  length(bytes) >= n && identical(bytes[seq_len(n)], as.raw(prefix))
}

renv_archive_list <- function(archive) {
  suppressWarnings(renv_archive_list_impl(archive))
}

renv_archive_list_impl <- function(archive) {

  switch(
    renv_archive_type(archive),
    tar = untar(archive, list = TRUE),
    zip = unzip(archive, list = TRUE)[["Name"]],
    stopf("don't know how to list files in archive '%s'", basename(archive))
  )

}

renv_archive_decompress <- function(archive, files = NULL, exdir = ".", ...) {

  switch(
    renv_archive_type(archive),
    tar = renv_archive_decompress_tar(archive, files = files, exdir = exdir, ...),
    zip = renv_archive_decompress_zip(archive, files = files, exdir = exdir, ...),
    stopf("don't know how to decompress archive '%s'", basename(archive))
  )

}

renv_archive_decompress_tar <- function(archive, files = NULL, exdir = ".", ...) {

  # if an appropriate system tar is available, use it
  tar <- renv_tar_exe()
  if (nzchar(tar))
    return(renv_tar_decompress(tar, archive = archive, files = files, exdir = exdir, ...))

  # when using internal TAR, we want to suppress warnings
  # (otherwise we get noise about global PAX headers)
  suppressWarnings(untar(archive, files = files, exdir = exdir, tar = "internal", ...))
  return(TRUE)

}

renv_archive_decompress_zip <- function(archive, files = NULL, exdir = ".", ...) {

  # the default unzip tool will give warnings rather than
  # errors if R was unable to extract from a zip archive
  status <- tryCatch(
    unzip(archive, files = files, exdir = exdir, ...),
    condition = identity
  )

  if (inherits(status, "condition")) {
    fmt <- "failed to decompress '%s' [%s]"
    stopf(fmt, basename(archive), conditionMessage(status))
  }

  TRUE

}

renv_archive_find <- function(archive, pattern) {
  files <- renv_archive_list(archive)
  grep(pattern, files, value = TRUE)
}

renv_archive_read <- function(archive, file) {

  type <- renv_archive_type(archive)
  case(
    type == "tar" ~ renv_archive_read_tar(archive, file),
    type == "zip" ~ renv_archive_read_zip(archive, file),
    ~ stopf("don't know how to read file from archive %s", renv_path_pretty(archive))
  )

}

renv_archive_read_tar <- function(archive, file) {

  # if an appropriate tar is available, use it
  tar <- renv_tar_exe()
  if (nzchar(tar)) {
    args <- c("xf", renv_shell_path(archive), "-O", renv_shell_path(file))
    return(renv_system_exec(tar, args, action = "reading file from archive"))
  }

  # create extraction directory
  exdir <- renv_scope_tempfile("renv-archive-")
  ensure_directory(exdir)

  # unpack the requested file
  suppressWarnings(untar(archive, files = file, exdir = exdir, tar = "internal"))

  # and read it
  archive <- file.path(exdir, file)
  readLines(archive, warn = FALSE)

}

renv_archive_read_zip <- function(archive, file) {
  renv_scope_tempdir()
  conn <- unz(archive, file, encoding = "native.enc")
  defer(close(conn))
  readLines(conn, warn = FALSE)
}
