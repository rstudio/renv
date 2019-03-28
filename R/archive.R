
renv_archive_type <- function(path) {

  ext <- fileext(path)
  if (ext %in% c(".zip"))
    return("zip")
  else if (ext %in% c(".tgz", ".tar", ".tar.gz"))
    return("tar")
  else
    return("unknown")

}

renv_archive_list <- function(path) {

  switch(
    renv_archive_type(path),
    tar = untar(path, list = TRUE),
    zip = unzip(path, list = TRUE)[["Name"]],
    stopf("don't know how to list files in archive '%s'", basename(path))
  )

}

renv_archive_compressor <- function(path) {

  switch(
    renv_archive_type(path),
    tar = tar,
    zip = zip,
    stopf("cannot provide compressor for archive '%s'", basename(path))
  )

}

renv_archive_decompressor <- function(path) {

  switch(
    renv_archive_type(path),
    tar = untar,
    zip = unzip,
    stopf("cannot provide decompressor for archive '%s'", basename(path))
  )

}
