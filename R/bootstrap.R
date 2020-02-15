
bootstrap <- function(version, library) {

  # fix up repos
  repos <- getOption("repos")
  on.exit(options(repos = repos), add = TRUE)
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
  options(repos = repos)

  # attempt to download renv
  tarball <- tryCatch(renv_bootstrap_download(version), error = identity)
  if (inherits(tarball, "error"))
    stop("failed to download renv ", version)

  # now attempt to install
  status <- tryCatch(renv_bootstrap_install(version, tarball, library), error = identity)
  if (inherits(status, "error"))
    stop("failed to install renv ", version)

}

renv_bootstrap_download <- function(version) {

  methods <- list(
    renv_bootstrap_download_cran_latest,
    renv_bootstrap_download_cran_archive,
    renv_bootstrap_download_github
  )

  for (method in methods) {
    path <- tryCatch(method(version), error = identity)
    if (is.character(path) && file.exists(path))
      return(path)
  }

  stop("failed to download renv ", version)

}

renv_bootstrap_download_cran_latest <- function(version) {

  # check for renv on CRAN matching this version
  db <- as.data.frame(available.packages(), stringsAsFactors = FALSE)
  if (!"renv" %in% rownames(db))
    stop("renv is not available on your declared package repositories")

  entry <- db["renv", ]
  if (!identical(entry$Version, version))
    stop("renv is not available on your declared package repositories")

  message("* Downloading renv ", version, " from CRAN ... ", appendLF = FALSE)

  info <- tryCatch(
    download.packages("renv", destdir = tempdir()),
    condition = identity
  )

  if (inherits(info, "condition")) {
    message("FAILED")
    return(FALSE)
  }

  message("OK")
  info[1, 2]

}

renv_bootstrap_download_cran_archive <- function(version) {

  name <- sprintf("renv_%s.tar.gz", version)
  repos <- getOption("repos")
  urls <- file.path(repos, "src/contrib/Archive/renv", name)
  destfile <- file.path(tempdir(), name)

  message("* Attempting to download renv ", version, " from CRAN archive ... ", appendLF = FALSE)

  for (url in urls) {

    status <- tryCatch(
      download.file(url, destfile, mode = "wb", quiet = TRUE),
      condition = identity
    )

    if (identical(status, 0L)) {
      message("OK")
      return(destfile)
    }

  }

  message("FAILED")
  return(FALSE)

}

renv_bootstrap_download_github <- function(version) {

  enabled <- Sys.getenv("RENV_BOOTSTRAP_FROM_GITHUB", unset = "TRUE")
  if (!identical(enabled, "TRUE"))
    return(FALSE)

  # prepare download options
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(Sys.which("curl")) && nzchar(pat)) {
    fmt <- "--location --fail --header \"Authorization: token %s\""
    extra <- sprintf(fmt, pat)
    saved <- options("download.file.method", "download.file.extra")
    options(download.file.method = "curl", download.file.extra = extra)
    on.exit(do.call(base::options, saved), add = TRUE)
  } else if (nzchar(Sys.which("wget")) && nzchar(pat)) {
    fmt <- "--header=\"Authorization: token %s\""
    extra <- sprintf(fmt, pat)
    saved <- options("download.file.method", "download.file.extra")
    options(download.file.method = "wget", download.file.extra = extra)
    on.exit(do.call(base::options, saved), add = TRUE)
  }

  message("* Downloading renv ", version, " from GitHub ... ", appendLF = FALSE)

  url <- file.path("https://api.github.com/repos/rstudio/renv/tarball", version)
  name <- sprintf("renv_%s.tar.gz", version)
  destfile <- file.path(tempdir(), name)

  status <- tryCatch(
    download.file(url, destfile = destfile, mode = "wb", quiet = TRUE),
    condition = identity
  )

  if (!identical(status, 0L)) {
    message("FAILED")
    return(FALSE)
  }

  message("Done!")
  return(destfile)

}

renv_bootstrap_install <- function(version, tarball, library) {

  # attempt to install it into project library
  message("* Installing renv ", version, " ... ", appendLF = FALSE)
  dir.create(library, showWarnings = FALSE, recursive = TRUE)

  # invoke using system2 so we can capture and report output
  bin <- R.home("bin")
  exe <- if (Sys.info()[["sysname"]] == "Windows") "R.exe" else "R"
  r <- file.path(bin, exe)
  args <- c("--vanilla", "CMD", "INSTALL", "-l", shQuote(library), shQuote(tarball))
  output <- system2(r, args, stdout = TRUE, stderr = TRUE)
  message("Done!")

  # check for successful install
  status <- attr(output, "status")
  if (is.numeric(status) && !identical(status, 0L)) {
    header <- "Error installing renv:"
    lines <- paste(rep.int("=", nchar(header)), collapse = "")
    text <- c(header, lines, output)
    writeLines(text, con = stderr())
  }

  status

}

