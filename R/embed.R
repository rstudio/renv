
#' Capture and re-use dependencies within a `.R`, `.Rmd` or `.qmd`
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
#' When you next run your R script or render your `.Rmd` or `.qmd`, `use()` will:
#'
#' 1. Create a temporary library path,
#'
#' 1. Install the requested packages and their recursive dependencies into that
#'    library,
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
#' ## Package Resolution
#'
#' When `embed()` generates a call to `use()`, it needs to determine the
#' package versions to record. The `lockfile` parameter controls where these
#' versions come from:
#'
#' 1. If `lockfile` is `NULL` (the default), and the project has an existing
#'    `renv.lock` file, that project lockfile is used. Only packages present
#'    in the lockfile will be included -- any dependencies not recorded in the
#'    lockfile will be omitted.
#'
#' 2. If `lockfile` is a path to a lockfile, or a lockfile object, that
#'    lockfile is used directly.
#'
#' 3. If `lockfile` is `FALSE`, or if no project lockfile exists, package
#'    versions are inferred from the currently-installed packages in the
#'    active library paths.
#'
#' 4. If `lockfile` is `NA`, package versions are inferred from the
#'    latest versions available in the active package repositories.
#'
#' @param lockfile
#'   The path to an renv lockfile. When `NULL` (the default), the project
#'   lockfile will be read (if any); otherwise, a new lockfile will be generated
#'   from the current library paths. Use `lockfile = FALSE` to use
#'   currently-installed package versions, or `lockfile = NA` to use the
#'   latest versions available in the active package repositories. See
#'   **Package Resolution** for more details.
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
    ext == ".rmd" ~ renv_embed_rmd,
    ext == ".qmd" ~ renv_embed_rmd
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

renv_embed_create_lockfile <- function(path = NULL,
                                       lockfile = NULL,
                                       project = NULL)
{
  # figure out the package dependencies for this script
  deps <- dependencies(path, quiet = TRUE)
  packages <- sort(unique(deps[["Package"]]))
  all <- renv_package_dependencies(packages)

  # select the lockfile source
  source <- renv_embed_lockfile_select(lockfile, project)

  # notify user if some dependencies appear to be unavailable
  # (skip when using repository versions, since we don't need
  # packages to be installed in that case)
  if (!identical(source, NA)) {
    ok <- nzchar(all)
    missing <- names(all)[!ok]
    if (length(missing)) {
      missing <- sort(unique(missing))
      stop("required packages are not installed: ", paste(missing, collapse = ", "))
    }
  }
  lockfile <- renv_embed_lockfile_resolve(path, names(all), source, project)

  # keep only matched records
  renv_lockfile_records(lockfile) <-
    renv_lockfile_records(lockfile) %>%
    keep(c("renv", names(all)))

  # notify user if some dependencies are missing
  ignored <- c("renv", renv_packages_base())
  expected <- setdiff(names(all), ignored)
  actual <- names(renv_lockfile_records(lockfile))
  missing <- setdiff(expected, actual)
  if (length(missing)) {

    postamble <- if (identical(source, NA)) c(
      "These packages are not available in the active package repositories."
    ) else if (identical(source, FALSE)) c(
      "Packages must first be installed before renv can snapshot them."
    ) else c(
      "Use `renv::snapshot()` to update the lockfile, or",
      "use `renv::embed(lockfile = FALSE)` to use installed package versions instead."
    )

    bulletin(
      preamble = "The following required packages are not available:",
      values = sort(unique(missing)),
      postamble = postamble
    )

    renv_condition_signal("renv.embed.missing_packages", missing)

  }

  invisible(lockfile)
}

renv_embed_create <- function(path = NULL,
                              lockfile = NULL,
                              project = NULL)
{
  lockfile <- renv_embed_create_lockfile(path, lockfile, project)
  renv_lockfile_compact(lockfile)
}

renv_embed_r <- function(path, ..., lockfile = NULL, project = NULL) {

  # resolve project
  project <- renv_project_resolve(project, default = NULL)

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

  # insert new lockfile
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
  lockfile <- renv_embed_create_lockfile(path, lockfile, project)

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
  project <- renv_project_resolve(project, default = NULL)

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

  # insert new lockfile
  contents <- c(
    head(contents, n = start - 1L),
    embed,
    tail(contents, n = length(contents) - end)
  )

  writeLines(contents, con = path)
  return(TRUE)

}

renv_embed_lockfile_select <- function(lockfile, project) {

  # if lockfile is explicitly FALSE, use installed packages
  if (identical(lockfile, FALSE))
    return(FALSE)

  # if lockfile is NA, use available packages from repositories
  if (identical(lockfile, NA))
    return(NA)

  # if lockfile is a character path or list, use it directly
  if (is.character(lockfile) || is.list(lockfile))
    return(lockfile)

  # lockfile is NULL; check for a project lockfile
  if (length(project)) {
    path <- renv_lockfile_path(project)
    if (file.exists(path))
      return(path)
  }

  # no project lockfile; fall back to installed packages
  FALSE

}

renv_embed_lockfile_resolve <- function(path, packages, lockfile, project) {

  # if we have a lockfile path, read it
  if (is.character(lockfile))
    return(renv_lockfile_read(lockfile))

  # if we have a lockfile object, use it as-is
  if (is.list(lockfile))
    return(lockfile)

  # if NA, use available packages from repositories
  if (identical(lockfile, NA))
    return(renv_embed_lockfile_resolve_available(packages, project))

  # otherwise, generate a lockfile from installed packages
  project <- renv_project_resolve(project, default = NULL)
  snapshot(
    lockfile = NULL,
    packages = packages,
    project  = project
  )

}

renv_embed_lockfile_resolve_available <- function(packages, project) {

  project <- renv_project_resolve(project, default = NULL)
  lockfile <- renv_lockfile_init(project)

  # look up the latest version for each package
  ignored <- renv_packages_base()
  packages <- setdiff(packages, ignored)

  records <- map(packages, function(package) {
    catch(renv_available_packages_latest(package))
  })

  names(records) <- packages
  records <- filter(records, function(record) !inherits(record, "error"))
  renv_lockfile_records(lockfile) <- records

  lockfile

}
