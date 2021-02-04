
#' Embed a Lockfile
#'
#' Use `embed()` to embed a compact representation of an `renv` lockfile
#' directly within a file, using [use()] to automatically provision an
#' \R library when that script is run.
#'
#' Using `embed()` is useful if you'd like to be able to share "reproducible"
#' R scripts -- when these scripts are sourced, the generated call to
#' `renv::use()` will ensure that an R library with the requested packages
#' is automatically provisioned.
#'
#' @inheritParams renv-params
#'
#' @param path
#'   The path to an \R or R Markdown script.
#'
#' @param lockfile
#'   The path to an `renv` lockfile. When `NULL` (the default), the project
#'   lockfile will be read (if any); otherwise, a new lockfile will be generated
#'   from the current library paths.
embed <- function(path = NULL,
                  ...,
                  lockfile = NULL,
                  project = NULL)
{
  path <- path %||% renv_embed_path()

  ext <- tolower(fileext(path))
  method <- case(
    ext == ".r"   ~ renv_embed_r,
    ext == ".rmd" ~ renv_embed_rmd
  )

  if (is.null(method)) {
    fmt <- "don't know how to embed lockfile into file %s"
    stopf(fmt, renv_path_pretty(path))
  }

  delegate(method)
}

renv_embed_path <- function() {

  tryCatch(
    renv_embed_path_impl(),
    error = function(e) NULL
  )

}

renv_embed_path_impl <- function() {
  rstudio <- as.environment("tools:rstudio")
  rstudio$.rs.api.documentPath()
}

renv_embed_create <- function(path = NULL,
                              lockfile = NULL,
                              project = NULL)
{
  # generate lockfile
  project <- renv_project_resolve(project)

  lockfile <- if (is.character(lockfile))
    renv_lockfile_read(lockfile)
  else if (!is.null(lockfile))
    lockfile
  else if (file.exists(renv_lockfile_path(project)))
    renv_lockfile_load(project = project)
  else
    snapshot(project = project)

  # figure out recursive package dependencies
  deps <- dependencies(path, progress = FALSE)
  packages <- sort(unique(deps$Package))
  all <- renv_package_dependencies(packages)

  # keep only matched records
  lockfile$Packages <-
    keep(lockfile$Packages, c("renv", names(all)))

  # write compact use statement
  renv_lockfile_compact(lockfile)
}

renv_embed_r <- function(path, ..., lockfile = NULL, project = NULL) {

  # resolve project
  project <- renv_project_resolve(project)

  # read file contents
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # generate embed
  embed <- renv_embed_create(
    path = path,
    lockfile = lockfile,
    project = project
  )

  # check for existing 'renv::use' statement
  pattern <- "^\\s*(?:renv:{2,3})?use\\(\\s*$"
  index <- grep(pattern, contents, perl = TRUE)

  # if we don't have an index, just insert at start
  if (empty(index)) {
    contents <- c(embed, "", contents)
    writeLines(contents, con = path)
    return(TRUE)
  }

  # otherwise, try to replace an existing embedded lockfile
  start <- index

  # find the end of the block
  n <- length(contents)
  lines <- grep("^\\s*\\)\\s*$", contents, perl = TRUE)
  end <- min(lines[lines > start], n + 1L)

  # inject new lockfile
  contents <- c(
    head(contents, n = start - 1L),
    embed,
    tail(contents, n = n - end)
  )

  writeLines(contents, con = path)
  return(TRUE)

}

renv_embed_create_rmd <- function(path = NULL,
                                  lockfile = NULL,
                                  project = NULL)
{
  # create lockfile
  project <- renv_project_resolve(project)
  lockfile <- renv_lockfile_resolve(
    lockfile %||% snapshot(project = project, lockfile = NULL)
  )

  # create embed
  embed <- renv_embed_create(
    path = path,
    lockfile = lockfile,
    project = project
  )

  # return embed
  c("```{r renv, include=FALSE}", embed, "```")

}


renv_embed_rmd <- function(path,
                           ...,
                           lockfile = NULL,
                           project = NULL)
{
  # resolve project
  project <- renv_project_resolve(project)

  # read file contents
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # generate embed
  embed <- renv_embed_create_rmd(
    path = path,
    lockfile = lockfile,
    project = project
  )

  # check for existing renv.lock in file
  # if it exists, we'll want to replace at this location;
  # otherwise, insert at end of document
  header <- "^\\s*```{r renv"
  footer <- "```"
  start <- grep(header, contents, perl = TRUE)

  # if we don't have an index, insert after YAML header (if any)
  if (empty(start)) {
    bounds <- which(trimws(contents) == "---")

    all <- if (length(bounds) >= 2) {
      index <- bounds[[2L]]
      c(
        head(contents, n = index),
        "",
        embed,
        "",
        tail(contents, n = length(contents) - index)
      )
    } else {
      c(embed, "", contents)
    }

    writeLines(all, con = path)
    return(TRUE)
  }

  # otherwise, try to replace an existing embedded lockfile
  ends <- which(contents == footer)
  end <- min(ends[ends > start])

  # inject new lockfile
  contents <- c(
    head(contents, n = start - 1L),
    embed,
    tail(contents, n = length(contents) - end)
  )

  writeLines(contents, con = path)
  return(TRUE)

}
