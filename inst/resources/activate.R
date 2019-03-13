
local({

  # try to find a path where renv might be installed
  prefix <- file.path(R.version$platform, getRversion()[1, 1:2])
  base <- c(Sys.getenv("RENV_PATHS_ROOT", unset = "~/.renv"), "renv")
  paths <- file.path(base, "bootstrap", prefix, "renv/${VERSION}")

  # try to load renv from one of these paths
  for (path in paths)
    if (requireNamespace("renv", lib.loc = path, quietly = TRUE))
      return(renv::load())

  # failed to find renv locally; we'll try to install from GitHub.
  # first, set up download options as appropriate (try to use GITHUB_PAT)
  path <- paths[[1]]
  try({

    pat <- Sys.getenv("GITHUB_PAT")
    if (nzchar(Sys.which("curl")) && nzchar(pat)) {
      extra <- sprintf("-L -f -H \"Authorization: token %s\"", pat)
      saved <- options("download.file.method", "download.file.extra")
      options(download.file.method = "curl", download.file.extra = extra)
      on.exit(do.call(base::options, saved), add = TRUE)
    } else if (nzchar(Sys.which("wget")) && nzchar(pat)) {
      extra <- sprintf("--header=\"Authorization: token %s\"", pat)
      saved <- options("download.file.method", "download.file.extra")
      options(download.file.method = "wget", download.file.extra = extra)
      on.exit(do.call(base::options, saved), add = TRUE)
    }

    # try to download renv
    message("Downloading renv ${VERSION} ... ", appendLF = FALSE)
    url <- "https://api.github.com/repos/rstudio/renv/tarball/${VERSION}"
    destfile <- tempfile("renv-", fileext = ".tar.gz")
    on.exit(unlink(destfile), add = TRUE)
    utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
    message("Done!")

    # attempt to install it into bootstrap library
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    install.packages(destfile, repos = NULL, type = "source", lib = path, quiet = TRUE)

  })

  # try again to load
  if (requireNamespace("renv", lib.loc = path, quietly = TRUE))
    return(renv::load())

  # failed to download or load renv; warn the user
  msg <- c(
    "Failed to find an renv installation: the project will not be loaded.",
    "Use `renv::activate()` to re-initialize the project."
  )

  warning(paste(msg, collapse = "\n"), call. = FALSE)

})
