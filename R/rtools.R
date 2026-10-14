
renv_rtools_list <- function() {

  drive <- Sys.getenv("SYSTEMDRIVE", unset = "C:")

  roots <- c(

    renv_rtools_registry(),

    Sys.getenv("RTOOLS45_HOME", unset = file.path(drive, "rtools45")),
    Sys.getenv("RTOOLS44_HOME", unset = file.path(drive, "rtools44")),
    Sys.getenv("RTOOLS43_HOME", unset = file.path(drive, "rtools43")),
    Sys.getenv("RTOOLS42_HOME", unset = file.path(drive, "rtools42")),
    Sys.getenv("RTOOLS40_HOME", unset = file.path(drive, "rtools40")),
    file.path(drive, list.files(paste0(drive, "/"), pattern = "^rtools\\d\\d$")),
    file.path(drive, "Rtools"),
    list.files(file.path(drive, "RBuildTools"), full.names = TRUE),

    "~/Rtools",
    list.files("~/RBuildTools", full.names = TRUE)

  )

  roots <- unique(roots[file.exists(roots)])
  lapply(roots, renv_rtools_read)

}

renv_rtools_find <- function(rversion = getRversion()) {

  specs <- filter(renv_rtools_list(), renv_rtools_compatible, rversion = rversion)
  if (empty(specs))
    return(NULL)

  # several series can be compatible with one version of R, since each
  # serves newer releases until the next series appears; prefer the newest
  versions <- numeric_version(map_chr(specs, function(spec) format(spec$version)))
  specs[[order(versions, decreasing = TRUE)[[1L]]]]

}

renv_rtools_read <- function(root) {

  list(
    root    = root,
    version = renv_rtools_version(root)
  )

}

renv_rtools_version <- function(root) {

  name <- basename(root)

  # check for 'rtools<xyz>' folder
  # e.g. C:/rtools42
  pattern <- "^rtools(\\d)(\\d)$"
  if (grepl(pattern, name, perl = TRUE, ignore.case = TRUE))
    return(gsub(pattern, "\\1.\\2", name, perl = TRUE, ignore.case = TRUE))

  # check for versioned installation path
  # e.g. C:/RBuildTools/4.2
  version <- catch(numeric_version(name))
  if (!inherits(version, "error"))
    return(format(version))

  # detect older Rtools installations
  path <- file.path(root, "VERSION.txt")
  if (!file.exists(path))
    return(NULL)

  contents <- readLines(path, warn = FALSE)
  version <- gsub("[^[:digit:].]", "", contents)
  numeric_version(version)

}

renv_rtools_compatible <- function(spec, rversion = getRversion()) {

  if (is.null(spec$version))
    return(FALSE)

  ranges <- list(
    "4.5" = c("4.5.0", "9.9.9"),
    "4.4" = c("4.4.0", "9.9.9"),
    "4.3" = c("4.3.0", "4.4.0"),
    "4.2" = c("4.2.0", "4.3.0"),
    "4.0" = c("4.0.0", "4.2.0"),
    "3.5" = c("3.3.0", "4.0.0"),
    "3.4" = c("3.3.0", "4.0.0"),
    "3.3" = c("3.2.0", "3.3.0"),
    "3.2" = c("3.1.0", "3.2.0"),
    "3.1" = c("3.0.0", "3.1.0")
  )

  version <- numeric_version(spec$version)[1, 1:2]
  rversion <- numeric_version(rversion)

  # newer series than those listed above follow the same pattern as
  # Rtools 4.2 onwards: they support R from the matching minor release
  range <- ranges[[format(version)]]
  if (is.null(range))
    return(version >= "4.6" && rversion >= version)

  range[[1]] <= rversion && rversion < range[[2]]

}

renv_rtools_registry <- function() {

  status <- tryCatch(
    utils::readRegistry(
      key = "SOFTWARE\\R-Core\\Rtools",
      hive = "HLM"
    ),
    error = function(e) list()
  )

  path <- status$InstallPath %||% ""
  if (file.exists(path))
    return(renv_path_normalize(path))

}

renv_rtools_envvars <- function(root) {

  version <- renv_rtools_version(root)

  if (version < "4.0")
    renv_rtools_envvars_default(root)
  else if (version < "4.2")
    renv_rtools_envvars_rtools40(root)
  else if (version < "4.3")
    renv_rtools_envvars_rtools42(root)
  else if (version < "4.4")
    renv_rtools_envvars_rtools43(root)
  else
    renv_rtools_envvars_default(root)

}

renv_rtools_envvars_default <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- ""

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools43 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- ""

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools42 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)

  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- ""

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools40 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF (note: trailing slash is required)
  binpref <- "/mingw$(WIN)/bin/"

  list(PATH = path, BINPREF = binpref)

}

# the Rtools series known to be available from CRAN; used when the
# list can't be read from CRAN itself
renv_rtools_series <- function() {
  c("4.0", "4.2", "4.3", "4.4", "4.5")
}

# the Rtools series (e.g. "4.5") appropriate for a version of R: the newest
# series which is not newer than R itself, as each series supports the
# matching minor release of R, and (until the next series) newer releases
renv_rtools_required <- function(version = getRversion(),
                                 series = renv_rtools_series())
{
  version <- numeric_version(version)
  if (version < "4.0.0")
    return("3.5")

  # a series newer than R itself doesn't support it, so when none of the
  # series provided are old enough (e.g. an index page which has dropped the
  # oldest series), fall back to the known series instead
  series <- sort(numeric_version(series))
  candidates <- series[series <= version[1, 1:2]]
  if (empty(candidates))
    return(renv_rtools_required(version, renv_rtools_series()))

  format(max(candidates))
}

# the URL of the CRAN page describing a series of Rtools
renv_rtools_page_url <- function(series) {

  root <- "https://cran.r-project.org/bin/windows/Rtools"
  nn <- gsub(".", "", series, fixed = TRUE)

  # Rtools 4.0 is documented on a single page; later series have their own
  if (series == "4.0")
    file.path(root, sprintf("rtools%s.html", nn))
  else
    file.path(root, sprintf("rtools%s", nn), "rtools.html")

}

# the Rtools series listed on the CRAN Rtools index page
renv_rtools_series_available <- function() {

  root <- "https://cran.r-project.org/bin/windows/Rtools"
  destfile <- renv_scope_tempfile("renv-rtools-", fileext = ".html")
  html <- catch(download(file.path(root, "index.html"), destfile, quiet = TRUE))
  if (inherits(html, "error"))
    return(renv_rtools_series())

  series <- renv_rtools_series_parse(readLines(destfile, warn = FALSE))
  if (empty(series))
    return(renv_rtools_series())

  series

}

renv_rtools_series_parse <- function(html) {

  pattern <- "rtools(\\d)(\\d)(?:/rtools)?[.]html"
  matches <- regmatches(html, gregexpr(pattern, html, perl = TRUE))
  links <- unique(unlist(matches, use.names = FALSE))

  series <- gsub(pattern, "\\1.\\2", links, perl = TRUE)
  format(sort(numeric_version(series)))

}

# the URL of the installer for a series of Rtools, as linked from its page
# on CRAN; the installer file names include build numbers which change
# over time, so they have to be read from the page
renv_rtools_installer_url <- function(series, arch = R.version$arch) {

  pageurl <- renv_rtools_page_url(series)
  destfile <- renv_scope_tempfile("renv-rtools-", fileext = ".html")
  download(pageurl, destfile, quiet = TRUE)
  html <- readLines(destfile, warn = FALSE)

  url <- renv_rtools_installer_url_parse(html, series, arch)
  if (is.null(url))
    stopf("no Rtools %s installer for '%s' was found at %s", series, arch, pageurl)

  # links on the page are relative to the page itself
  if (!grepl("^\\w+://", url))
    url <- file.path(dirname(pageurl), url)

  url

}

renv_rtools_installer_url_parse <- function(html, series, arch = R.version$arch) {

  pattern <- "href=\"([^\"]*rtools[^\"/]*[.]exe)\""
  matches <- regmatches(html, gregexpr(pattern, html, perl = TRUE))
  links <- gsub(pattern, "\\1", unique(unlist(matches, use.names = FALSE)), perl = TRUE)

  # only installers for the requested series are wanted
  names <- basename(links)
  prefix <- sprintf("rtools%s", gsub(".", "", series, fixed = TRUE))
  links <- links[startsWith(names, prefix)]
  if (empty(links))
    return(NULL)

  # installers for other architectures are distinguished by name;
  # the x86_64 installer is the unmarked one on newer pages
  names <- basename(links)
  aarch64 <- grepl("aarch64", names, fixed = TRUE)
  i686 <- grepl("i686", names, fixed = TRUE)

  candidates <- case(
    arch == "aarch64" ~ links[aarch64],
    arch == "i386"    ~ links[i686],
    ~ links[!aarch64 & !i686]
  )

  if (empty(candidates))
    return(NULL)

  candidates[[1L]]

}

# is a build toolchain available for use with this version of R?
renv_rtools_available <- function() {

  # an explicitly-requested or discovered Rtools installation
  root <- renv_paths_rtools()
  if (nzchar(root) && file.exists(root))
    return(TRUE)

  # otherwise, check for a toolchain the user has placed on the PATH
  renv_rtools_path_available()

}

# is a build toolchain (e.g. Rtools, or MSYS2) available on the PATH?
renv_rtools_path_available <- function() {
  make <- Sys.getenv("MAKE", unset = "make")
  nzchar(Sys.which(make)) && nzchar(Sys.which("gcc"))
}
