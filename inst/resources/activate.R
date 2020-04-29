
local({

  # the requested version of renv
  version <- "${VERSION}"

  # the project directory
  project <- getwd()

  # avoid recursion
  if (!is.na(Sys.getenv("RENV_R_INITIALIZING", unset = NA)))
    return(invisible(TRUE))

  # signal that we're loading renv during R startup
  Sys.setenv("RENV_R_INITIALIZING" = "true")
  on.exit(Sys.unsetenv("RENV_R_INITIALIZING"), add = TRUE)

  # signal that we've consented to use renv
  options(renv.consent = TRUE)

  # load the 'utils' package eagerly -- this ensures that renv shims, which
  # mask 'utils' packages, will come first on the search path
  library(utils, lib.loc = .Library)

  # check to see if renv has already been loaded
  if ("renv" %in% loadedNamespaces()) {

    # if renv has already been loaded, and it's the requested version of renv,
    # nothing to do
    spec <- .getNamespaceInfo(.getNamespace("renv"), "spec")
    if (identical(spec[["version"]], version))
      return(invisible(TRUE))

    # otherwise, unload and attempt to load the correct version of renv
    unloadNamespace("renv")

  }

  # tools for bootstrapping renv   
  bootstrap <- function(version, library) {
  
    # fix up repos
    repos <- getOption("repos")
    on.exit(options(repos = repos), add = TRUE)
    repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
    options(repos = repos)
  
    # attempt to download renv
    tarball <- tryCatch(
      renv_bootstrap_download(version),
      error = identity
    )
  
    if (inherits(tarball, "error"))
      stop("failed to download renv ", version)
  
    # now attempt to install
    status <- tryCatch(
      renv_bootstrap_install(version, tarball, library),
      error = identity
    )
  
    if (inherits(status, "error"))
      stop("failed to install renv ", version)
  
  }
  
  renv_bootstrap_download_impl <- function(url, destfile) {
  
    mode <- "wb"
  
    # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17715
    fixup <-
      Sys.info()[["sysname"]] == "Windows" &&
      substring(url, 1L, 5L) == "file:"
  
    if (fixup)
      mode <- "w+b"
  
    download.file(
      url      = url,
      destfile = destfile,
      mode     = mode,
      quiet    = TRUE
    )
  
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
      stop("renv is not available from your declared package repositories")
  
    entry <- db["renv", ]
    if (!identical(entry$Version, version))
      stop("renv is not available from your declared package repositories")
  
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
  
    message("* Downloading renv ", version, " from CRAN archive ... ", appendLF = FALSE)
  
    for (url in urls) {
  
      status <- tryCatch(
        renv_bootstrap_download_impl(url, destfile),
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
      renv_bootstrap_download_impl(url, destfile),
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
  
  renv_bootstrap_os_release <- function(path) {
  
    # this is a 'creative' use of read.table, but it works
    release <- read.table(
      file         = path,
      header       = FALSE,
      sep          = "=",
      comment.char = "#",
      col.names    = c("key", "value")
    )
  
    # extract into named vector
    vars <- as.list(release$value)
    names(vars) <- release$key
  
    # extract OS id
    id <- vars$ID
    if (is.null(id)) {
      fmt <- "no declared ID in '%s'"
      stop(sprintf(fmt, path), call. = FALSE)
    }
  
    candidates <- c(
      "VERSION_CODENAME",
      "VERSION_ID"
    )
  
    for (candidate in candidates) {
      entry <- vars[[candidate]]
      if (!is.null(entry))
        return(paste(id, entry, sep = "-"))
    }
  
    fmt <- "could not infer operating system via release file '%s'"
    stop(sprintf(fmt, path), call. = FALSE)
  
  }
  
  renv_bootstrap_os <- function() {
  
    # allow override by user
    os <- Sys.getenv("RENV_OPERATING_SYSTEM", unset = NA)
    if (!is.na(os))
      return(os)
  
    # detect Windows
    if (Sys.info()[["sysname"]] == "Windows")
      return("windows")
  
    # detect macOS
    if (Sys.info()[["sysname"]] == "Darwin")
      return("macos")
  
    # detect Linux variants via release file
    if (file.exists("/etc/os-release")) {
  
      os <- tryCatch(
        renv_bootstrap_os_release("/etc/os-release"),
        error = identity
      )
  
      if (!inherits(os, "error"))
        return(os)
  
    }
  
    msg <- "
  Failed to infer your operating system! Please file a bug report via:
  
      utils::bug.report(package = \"renv\")
  
  You can also set the RENV_OPERATING_SYSTEM environment variable
  to explicitly tell renv the name of your operating system.
  "
  
    warning(msg)
  
    "unknown"
  
  }
  
  renv_bootstrap_library_root <- function(project) {
  
    path <- Sys.getenv("RENV_PATHS_LIBRARY", unset = NA)
    if (!is.na(path))
      return(path)
  
    path <- Sys.getenv("RENV_PATHS_LIBRARY_ROOT", unset = NA)
    if (!is.na(path))
      return(file.path(path, basename(project)))
  
    file.path(project, "renv/library")
  
  }
  
  renv_bootstrap_prefix <- function(os = TRUE) {
  
    # construct r component
    r <- paste("R", getRversion()[1, 1:2], sep = "-")
  
    # include SVN revision for development versions of R
    # (to avoid sharing platform-specific artefacts with released versions of R)
    devel <-
      identical(R.version[["status"]],   "Under development (unstable)") ||
      identical(R.version[["nickname"]], "Unsuffered Consequences")
  
    if (devel)
      r <- paste(r, R.version[["svn rev"]], sep = "-r")
  
    # construct library prefix (include os if requested)
    components <- c(if (os) renv_bootstrap_os(), r, R.version$platform)
    paste(components, collapse = "/")
  
  }
  
  renv_bootstrap_libpath_impl <- function(project, os) {
    file.path(
      renv_bootstrap_library_root(project),
      renv_bootstrap_prefix(os)
    )
  }
  
  renv_bootstrap_libpath <- function(project) {
  
    # NOTE: older versions of renv did not include the operating system
    # name / version in the library or cache paths; try to transparently
    # migrate as necessary
  
    # construct path to current library path and use if it already exists
    newpath <- renv_bootstrap_libpath_impl(project, os = TRUE)
    if (file.exists(newpath))
      return(newpath)
  
    # check for old library path; if it doesn't exist we can just use the
    # new library path; otherwise we'll attempt to migrate
    oldpath <- renv_bootstrap_libpath_impl(project, os = FALSE)
    if (!file.exists(oldpath))
      return(newpath)
  
    # old library path exists but new doesn't; try to move
    source <- dirname(oldpath)
    target <- dirname(newpath)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    status <- tryCatch(file.rename(source, target), condition = identity)
    if (file.exists(newpath))
      return(newpath)
  
    # migration failed; continue to use previous library path
    oldpath
  
  }
  
  renv_bootstrap_version_check <- function(version) {
  
    # compare loaded version of renv with requested version
    loadedversion <- utils::packageDescription("renv", fields = "Version")
    if (version == loadedversion)
      return(TRUE)
  
    # assume four-component versions are from GitHub; three-component
    # versions are from CRAN
    components <- strsplit(loadedversion, "[.-]")[[1]]
    remote <- if (length(components) == 4L)
      paste("rstudio/renv", loadedversion, sep = "@")
    else
      paste("renv", loadedversion, sep = "@")
  
    fmt <- paste(
      "renv %1$s was loaded from project library, but renv %2$s is recorded in lockfile.",
      "Use `renv::record(\"%3$s\")` to record this version in the lockfile.",
      "Use `renv::restore(packages = \"renv\")` to install renv %2$s into the project library.",
      sep = "\n"
    )
  
    msg <- sprintf(fmt, loadedversion, version, remote)
    warning(msg, call. = FALSE)
  
    FALSE
  
  }
  
  renv_bootstrap_load <- function(project, version) {
  
    # attempt to load renv
    libpath <- renv_bootstrap_libpath(project)
    if (!requireNamespace("renv", lib.loc = libpath, quietly = TRUE))
      return(FALSE)
  
    # warn if the version of renv loaded does not match
    renv_bootstrap_version_check(version)
  
    # load the project
    renv::load(project = project)
  
    return(TRUE)
  
  }

  # attempt to load renv
  if (renv_bootstrap_load(project, version))
    return(TRUE)

  # couldn't load renv; try to bootstrap an installation
  bootstrap(version, libpath)

  # try again to load
  if (requireNamespace("renv", lib.loc = libpath, quietly = TRUE)) {
    message("Successfully installed and loaded renv ", version, ".")
    return(renv::load())
  }

  # failed to download or load renv; warn the user
  msg <- c(
    "Failed to find an renv installation: the project will not be loaded.",
    "Use `renv::activate()` to re-initialize the project."
  )

  warning(paste(msg, collapse = "\n"), call. = FALSE)

})
