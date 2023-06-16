renv_test_state <- function() {

  list_files <- function(path, full.names = TRUE) {

    if (is.null(path))
      return(NULL)

    list.files(
      path = path,
      all.files = TRUE,
      full.names = full.names,
      no.. = TRUE
    )

  }

  repopath <- renv_tests_repopath()
  userpath <- file.path(renv_bootstrap_user_dir(), "library")

  opts <- options()
  opts <- opts[grep("^diffobj", names(opts), invert = TRUE)]
  opts$ambiguousMethodSelection <- NULL
  opts$restart <- NULL
  opts$repos[opts$repos == "@CRAN@"] <- "https://cloud.r-project.org"
  opts <- opts[csort(names(opts))]

  envvars <- as.list(Sys.getenv())
  envvars <- envvars[grep("^RENV_DEFAULT_", names(envvars), invert = TRUE)]
  envvars <- envvars[grep("^R_PACKRAT_", names(envvars), invert = TRUE)]
  envvars <- envvars[grep("^_R_", names(envvars), invert = TRUE)]
  envvars <- envvars[grep("^CALLR_", names(envvars), invert = TRUE)]
  envvars$RETICULATE_MINICONDA_PYTHON_ENVPATH <- NULL
  envvars$OMP_NUM_THREADS <- NULL
  envvars$OPENBLAS <- NULL
  envvars <- envvars[csort(names(envvars))]

  tempfiles <- list_files(tempdir(), full.names = FALSE)
  tempfiles <- tempfiles[grep("^libloc_", tempfiles, invert = TRUE)]
  tempfiles <- tempfiles[grep("^callr-", tempfiles, invert = TRUE)]
  tempfiles <- tempfiles[grep("^repos_http(s?)", tempfiles, invert = TRUE)]
  tempfiles <- tempfiles[tempfiles != "downloaded_packages"]

  list(
    libpaths     = .libPaths(),
    connections  = getAllConnections(),
    options      = opts,
    repofiles    = list_files(repopath),
    userfiles    = list_files(userpath),
    # tempfiles    = tempfiles,
    envvars      = envvars
  )

}
set_state_inspector(renv_test_state)
