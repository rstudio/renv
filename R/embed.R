#' Capture and re-use dependencies within a `.R` or `.Rmd`
#'
#' @description
#' Together, `embed()` and `use()` provide a lightweight way to specify and
#' restore package versions within a file. `use()` is a lightweight lockfile
#' specification that `embed()` can automatically generate and insert into a
#' script or document.
#'
#' Calling `embed()` inspects the dependencies of the specified document then
#' generates and inserts a call to `use()` that looks something like this:
#'
#' ```R
#' renv::use(
#'   "digest@0.6.30",
#'   "rlang@0.3.4"
#' )
#' ```
#'
#' Then, when you next run your R script or render your `.Rmd`, `use()` will:
#'
#' 1. Create a temporary library path.
#'
#' 1. Install the requested packages and their recursive dependencies into that
#'    library.
#'
#' 1. Activate the library, so it's used for the rest of the script.
#'
#' ## Manual usage
#'
#' You can also create calls to `use()` yourself, either specifying the
#' packages needed by hand, or by supplying the path to a lockfile,
#' `renv::use(lockfile = "/path/to/renv.lock")`.
#'
#' This can be useful in projects where you'd like to associate different
#' lockfiles with different documents, as in a blog where you want each
#' post to capture the dependencies at the time of writing. Once you've
#' finished writing each, the post, you can use
#' `renv::snapshot(lockfile = "/path/to/renv.lock")`
#' to "save" the state that was active while authoring that bost, and then use
#' `renv::use(lockfile = "/path/to/renv.lock")` in that document to ensure the
#' blog post always uses those dependencies onfuture renders.
#'
#' `renv::use()` is inspired in part by the [groundhog](https://groundhogr.com/)
#' package, which also allows one to specify a script's \R package requirements
#' within that same \R script.
#'
#' @inherit renv-params
#'
#' @param path
#'   The path to an \R or R Markdown script. The default will use the current
#'   document, if running within RStudio.
#'
#' @param lockfile
#'   The path to an renv lockfile. When `NULL` (the default), the project
#'   lockfile will be read (if any); otherwise, a new lockfile will be generated
#'   from the current library paths.
#'
#' @export
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

  method(
    path     = path,
    lockfile = lockfile,
    project  = project,
    ...
  )

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
  lockfile <- renv_embed_lockfile_resolve(lockfile, project)

  # figure out recursive package dependencies
  deps <- renv_dependencies_impl(path)
  packages <- sort(unique(deps$Package))
  all <- renv_package_dependencies(packages)

  # keep only matched records
  lockfile$Packages <- keep(lockfile$Packages, c("renv", names(all)))

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
    path     = path,
    lockfile = lockfile,
    project  = project
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
  project  <- renv_project_resolve(project)
  lockfile <- renv_embed_lockfile_resolve(lockfile, project)

  # create embed
  embed <- renv_embed_create(
    path     = path,
    lockfile = lockfile,
    project  = project
  )

  # return embed
  c("```{r lockfile, include=FALSE}", embed, "```")

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
    path     = path,
    lockfile = lockfile,
    project  = project
  )

  # check for existing renv.lock in file
  # if it exists, we'll want to replace at this location;
  # otherwise, insert at end of document
  header <- "^\\s*```{r lockfile"
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

renv_embed_lockfile_resolve <- function(lockfile, project) {

  # if lockfile is character, assume it's the path to a lockfile
  if (is.character(lockfile))
    return(renv_lockfile_read(lockfile))

  # if lockfile is not NULL, assume lockfile object
  if (!is.null(lockfile))
    return(lockfile)

  # check for lockfile in project
  path <- renv_lockfile_path(project)
  if (file.exists(path))
    return(renv_lockfile_read(path))

  # no lockfile available; just snapshot
  snapshot(project = project, lockfile = NULL)

}
