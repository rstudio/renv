
#' Embed a Lockfile
#'
#' Use `embed()` to embed a lockfile directly within a file.
#'
#' This is primarily useful in tandem with [run] -- if you call `renv::run()`
#' on a script containing an inline lockfile, `renv` will first provision
#' a library based on that lockfile definition, and then run the script
#' using that lockfile.
#'
#' @inheritParams renv-params
#'
#' @param path The path to an \R or R Markdown script.
#'
embed <- function(path = NULL,
                  ...,
                  project = NULL)
{
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

renv_embed_create_r <- function(lockfile = NULL, project = NULL) {

  # generate lockfile
  project <- renv_project_resolve(project)
  lockfile <- lockfile %||% snapshot(project = project, lockfile = NULL)

  # write to JSON string
  embed <- renv_lockfile_write(lockfile, file = NULL)

  # comment each line in the lockfile
  splat <- strsplit(embed, "\n", fixed = TRUE)[[1]]
  body <- paste("#", splat)

  # create header
  header <- header("renv.lock", suffix = "-")
  c(header, "#", body)

}

renv_embed_r <- function(path, ..., project = NULL) {

  # resolve project
  project <- renv_project_resolve(project)

  # read file contents
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # generate a lockfile
  embed <- renv_embed_create_r(project = project)

  # check for existing renv.lock marker in file
  # if it exists, we'll want to replace at this location;
  # otherwise, insert at end of document
  pattern <- "#\\s*renv[.]lock\\s*-+"
  index <- grep(pattern, contents, perl = TRUE)

  # if we don't have an index, just insert at end
  if (empty(index)) {
    contents <- c(contents, "", embed)
    writeLines(contents, con = path)
    return(TRUE)
  }

  # otherwise, try to replace an existing embedded lockfile
  start <- index

  # find the end of the block
  n <- length(contents)
  lines <- grep("^#", contents, invert = TRUE)
  end <- min(lines[lines > start], n + 1L)

  # inject new lockfile
  contents <- c(
    head(contents, n = start - 1L),
    embed,
    tail(contents, n = n - end + 1L)
  )

  writeLines(contents, con = path)
  return(TRUE)

}

renv_embed_create_rmd <- function(lockfile = NULL, project = NULL) {

  # create lockfile
  project <- renv_project_resolve(project)
  lockfile <- snapshot(project = project, lockfile = NULL)

  # write to JSON
  body <- renv_lockfile_write(lockfile, file = NULL)
  body <- strsplit(body, "\n", fixed = TRUE)[[1L]]

  # surround in script tags
  header <- "<script id=\"renv-lockfile\" type=\"application/json\">"
  footer <- "</script>"

  # return embed
  c(header, body, footer)

}


renv_embed_rmd <- function(path, ..., project = NULL) {

  # resolve project
  project <- renv_project_resolve(project)

  # read file contents
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # generate embed
  embed <- renv_embed_create_rmd(project = project)

  # check for existing renv.lock in file
  # if it exists, we'll want to replace at this location;
  # otherwise, insert at end of document
  header <- "<script id=\"renv-lockfile\" type=\"application/json\">"
  footer <- "</script>"
  start <- which(contents == header)

  # if we don't have an index, just insert at end
  if (empty(start)) {
    contents <- c(contents, "", embed)
    writeLines(contents, con = path)
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
