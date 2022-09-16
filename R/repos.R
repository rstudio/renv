
`_renv_repos` <- new.env(parent = emptyenv())

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
    "-f", renv_shell_path(script),
    "--args", renv_shell_path(file), renv_shell_path(tempdir())
  )

  system2(R(), args, stdout = FALSE, stderr = FALSE, wait = FALSE)
  return(FALSE)

}

renv_repos_normalize <- function(repos = getOption("repos")) {

  # ensure repos are a character vector
  repos <- convert(repos, "character")

  # force a CRAN mirror when needed
  cran <- getOption("renv.repos.cran", "https://cloud.r-project.org")
  repos[repos == "@CRAN@"] <- cran

  # if repos is length 1 but has no names, then assume it's CRAN
  nms <- names(repos) %||% rep.int("", length(repos))
  if (identical(nms, ""))
    nms <- names(repos) <- "CRAN"

  # ensure all values are named
  unnamed <- !nzchar(nms)
  if (any(unnamed)) {
    nms[unnamed] <- paste0("V", seq_len(sum(unnamed)))
    names(repos) <- nms
  }

  # return normalized repository
  repos

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

renv_repos_info <- function(url) {

  memoize(
    key   = url,
    expr  = renv_repos_info_impl(url),
    envir = `_renv_repos`
  )

}

renv_repos_info_impl <- function(url) {

  # make sure the repository URL includes a trailing slash
  url <- gsub("/*$", "/", url)

  # if this is a file repository, return early
  if (grepl("^file:", url))
    return(list(nexus = FALSE))

  # try to download it
  destfile <- renv_scope_tempfile("renv-repos-")
  status <- catch(download(url, destfile = destfile, quiet = TRUE))
  if (inherits(status, "error"))
    return(status)

  # read the contents of the page
  contents <- renv_file_read(destfile)

  # determine if this is a Nexus repository
  nexus <-
    grepl("Nexus Repository Manager", contents, fixed = TRUE) ||
    grepl("<div class=\"nexus-header\">", contents, fixed = TRUE)

  list(
    nexus = nexus
  )

}
