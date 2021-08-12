
renv_repos_encode <- function(x) {
  if (length(x) == 1)
    paste(names(x), as.character(x), sep = "=")
  else
    paste(sprintf("\n\t%s=%s", names(x), as.character(x)), collapse = "")
}

renv_repos_decode <- function(x) {
  parts <- strsplit(trimws(x), "(?:,|\\s)+")[[1]]
  idx <- regexpr("=", parts, fixed = TRUE)
  keys <- substring(parts, 1, idx - 1)
  vals <- substring(parts, idx + 1)
  as.list(named(trimws(vals), trimws(keys)))
}

renv_repos_init_callback <- function(...) {

  # bail unless opted in
  config <- renv_config_get("eager.repos", default = FALSE)
  if (!identical(config, TRUE))
    return(FALSE)

  # write required data to file
  file <- tempfile("renv-repos-", fileext = ".rds")
  data <- list(repos = getOption("repos"), type = renv_package_pkgtypes())
  saveRDS(data, file = file, version = 2L)

  # invoke helper script
  script <- system.file("resources/scripts-repos-cache.R", package = "renv")
  args <- c(
    "--vanilla", "-s",
    "-f", shQuote(script),
    "--args", shQuote(file), shQuote(tempdir())
  )

  system2(R(), args, stdout = FALSE, stderr = FALSE, wait = FALSE)
  return(FALSE)

}

renv_repos_normalize <- function(repos = getOption("repos")) {

  # force a CRAN mirror when needed
  repos[repos == "@CRAN@"] <- getOption(
    "renv.repos.cran",
    "https://cloud.r-project.org"
  )

  # ensure repos are a character vector
  convert(repos, "character")

}

renv_repos_validate <- function(repos = getOption("repos")) {

  # allow empty repository explicitly
  if (empty(repos))
    return(character())

  # otherwise, ensure it's a named list or character vector
  ok <- is.list(repos) || is.character(repos)
  if (!ok)
    stopf("repos has unexpected type '%s'", typeof(repos))

  # read repository names
  nm <- names(repos) %||% rep.int("", length(repos))
  if (any(nm %in% "")) {

    # if this is a length-one repository, assume it's CRAN
    if (length(repos) == 1L) {
      repos <- c(CRAN = repos)
      return(renv_repos_normalize(repos))
    }

    # otherwise, error
    stopf("all repository entries must be named")

  }

  # normalize the repos option
  renv_repos_normalize(repos)

}
