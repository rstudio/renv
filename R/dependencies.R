
`_renv_dependencies` <- new.env(parent = emptyenv())

#' Find R Package Dependencies in a Project
#'
#' Find \R packages used within a project.
#'
#' `dependencies()` will crawl files within your project, looking for \R files
#' and the packages used within those \R files. This is done primarily by
#' parsing the code and looking for calls of the form:
#'
#' - `library(package)`
#' - `require(package)`
#' - `requireNamespace("package")`
#' - `package::method()`
#'
#' For \R package projects, dependencies expressed in the `DESCRIPTION` file
#' will also be discovered. Note that the `rmarkdown` package is required in
#' order to crawl dependencies in R Markdown files.
#'
#' @section Suppressing Errors:
#'
#' Depending on how you've structured your code, `renv` may emit errors when
#' attempting to enumerate dependencies within `.Rmd` / `.Rnw` documents.
#' For code chunks that you'd explicitly like `renv` to ignore, you can
#' include `renv.ignore=TRUE` in the chunk header. For example:
#'
#'     ```{r chunk-label, renv.ignore=TRUE}
#'     # code in this chunk will be ignored by renv
#'     ```
#'
#' Similarly, if you'd like `renv` to parse a chunk that is otherwise ignored
#' (e.g. because it has `eval=FALSE` as a chunk header), you can set:
#'
#'     ```{r chunk-label, eval=FALSE, renv.ignore=FALSE}
#'     # code in this chunk will _not be ignored
#'     ```
#'
#' @section Ignoring Files:
#'
#' By default, `renv` will read your project's `.gitignore`s (if any) to
#' determine whether certain files or folders should be included when traversing
#' directories. If preferred, you can also create a `.renvignore` file (with
#' entries of the same format as a standard `.gitignore` file) to tell `renv`
#' which files to ignore within a directory. If both `.renvignore` and
#' `.gitignore` exist within a folder, the `.renvignore` will be used in lieu of
#' the `.gitignore`.
#'
#' See <https://git-scm.com/docs/gitignore> for documentation on the
#' `.gitignore` format. Some simple examples here:
#'
#' ```
#' # ignore all R Markdown files
#' *.Rmd
#'
#' # ignore all data folders
#' data/
#'
#' # ignore only data folders from the root of the project
#' /data/
#' ```
#'
#' @inheritParams renv-params
#'
#' @param path The path to a (possibly multi-mode) \R file, or a directory
#'   containing such files. By default, all files within the current working
#'   directory are checked, recursively.
#'
#' @param root The root directory to be used for dependency discovery.
#'   Defaults to the active project directory. You may need to set this
#'   explicitly to ensure that your project's `.renvignore`s (if any) are
#'   properly handled.
#'
#' @param progress Boolean; report progress output while enumerating
#'   dependencies?
#'
#' @param errors How should errors that occur during dependency enumeration be
#'   handled? See **Errors** for more details.
#'
#' @param dev Boolean; include 'development' dependencies as well? That is,
#'   packages which may be required during development but are unlikely to be
#'   required during runtime for your project. By default, only runtime
#'   dependencies are returned.
#'
#' @return An \R `data.frame` of discovered dependencies, mapping inferred
#'   package names to the files in which they were discovered.
#'
#' @section Errors:
#'
#' `renv`'s attempts to enumerate package dependencies in your project can fail
#' -- most commonly, because of parse errors in your \R code. The `errors`
#' parameter can be used to control how `renv` responds to errors that occur.
#'
#' \tabular{ll}{
#' **Name** \tab **Action** \cr
#' `"reported"` \tab Errors are reported to the user, but are otherwise ignored. \cr
#' `"fatal"`    \tab Errors are fatal and stop execution. \cr
#' `"ignored"`  \tab Errors are ignored and not reported to the user. \cr
#' }
#'
#' Depending on the structure of your project, you may want `renv` to ignore
#' errors that occur when attempting to enumerate dependencies. However, a more
#' robust solution would be to use an `.renvignore` file to tell `renv` not to
#' scan such files for dependencies, or to configure the project to require
#' explicit dependency management (`renv::settings$snapshot.type("explicit")`)
#' and enumerate your dependencies in a project `DESCRIPTION` file.
#'
#' @section Development Dependencies:
#'
#' `renv` attempts to distinguish between 'development' dependencies and
#' 'runtime' dependencies. For example, you might rely on e.g.
#' [devtools](https://cran.r-project.org/package=devtools) and
#' [roxygen2](https://cran.r-project.org/package=roxygen2) during development
#' for a project, but may not actually require these packages at runtime.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # find R package dependencies in the current directory
#' renv::dependencies()
#'
#' }
dependencies <- function(
  path = getwd(),
  root = NULL,
  ...,
  progress = TRUE,
  errors = c("reported", "fatal", "ignored"),
  dev = FALSE)
{
  renv_scope_error_handler()

  deps <- renv_dependencies_impl(
    path     = path,
    root     = root,
    progress = progress,
    errors   = errors,
    dev      = dev,
    ...
  )

  if (empty(deps) || nrow(deps) == 0L)
    return(renv_dependencies_list_empty())

  # drop NAs, and only keep 'dev' dependencies if requested
  deps <- deps[deps$Dev %in% c(dev, FALSE), ]

  deps
}

renv_dependencies_impl <- function(
  path = getwd(),
  root = NULL,
  ...,
  progress = TRUE,
  errors = c("reported", "fatal", "ignored"),
  dev = FALSE)
{
  # special case: if 'path' is a function, parse its body for dependencies
  if (is.function(path))
    return(renv_dependencies_discover_r(expr = body(path)))

  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)
  root <- root %||% renv_dependencies_root(path)

  # ignore errors when testing, unless explicitly asked for
  if (renv_tests_running() && missing(errors))
    errors <- "ignored"

  # check and see if we've pre-computed dependencies for this path, and
  # retrieve those pre-computed dependencies if so
  if (length(path) == 1)
    if (exists(path, envir = `_renv_dependencies`))
      return(get(path, envir = `_renv_dependencies`))

  renv_dependencies_begin(root = root)
  on.exit(renv_dependencies_end(), add = TRUE)

  dots <- list(...)
  if (identical(dots[["quiet"]], TRUE)) {
    progress <- FALSE
    errors <- "ignored"
  }

  files <- renv_dependencies_find(path, root)
  deps <- renv_dependencies_discover(files, progress, errors)
  renv_dependencies_report(errors)

  deps
}

renv_dependencies_root <- function(path = getwd()) {

  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)

  project <- Sys.getenv("RENV_PROJECT", unset = NA)
  if (!is.na(project) && all(renv_path_within(path, project)))
    return(project)

  roots <- uapply(path, renv_dependencies_root_impl)
  if (empty(roots))
    return(NULL)

  uniroot <- unique(roots)
  if (length(uniroot) > 1)
    return(NULL)

  uniroot

}

renv_dependencies_root_impl <- function(path) {

  renv_file_find(path, function(parent) {
    anchors <- c("DESCRIPTION", ".git", ".Rproj.user", "renv.lock", "renv")
    for (anchor in anchors)
      if (file.exists(file.path(parent, anchor)))
        return(parent)
  })

}

renv_dependencies_callback <- function(path) {

  cbname <- list(
    ".Rprofile"     = function(path) renv_dependencies_discover_r(path),
    "DESCRIPTION"   = function(path) renv_dependencies_discover_description(path),
    "_bookdown.yml" = function(path) renv_dependencies_discover_bookdown(path),
    "_pkgdown.yml"  = function(path) renv_dependencies_discover_pkgdown(path),
    "_quarto.yml"   = function(path) renv_dependencies_discover_quarto(path),
    "renv.lock"     = function(path) renv_dependencies_discover_renv_lock(path),
    "rsconnect"     = function(path) renv_dependencies_discover_rsconnect(path)
  )

  cbext <- list(
    ".rproj"       = function(path) renv_dependencies_discover_rproj(path),
    ".r"           = function(path) renv_dependencies_discover_r(path),
    ".qmd"         = function(path) renv_dependencies_discover_multimode(path, "qmd"),
    ".rmd"         = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rmarkdown"   = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rnw"         = function(path) renv_dependencies_discover_multimode(path, "rnw")
  )

  name <- basename(path)
  ext  <- tolower(fileext(path))

  callback <- cbname[[name]] %||% cbext[[ext]]
  if (!is.null(callback))
    return(callback)

  # for files without an extension, check if those might be executable by R
  if (!nzchar(ext)) {
    shebang <- renv_file_shebang(path)
    if (grepl("\\b(?:R|r|Rscript)\\b", shebang))
      return(function(path) renv_dependencies_discover_r(path))
  }

}

renv_dependencies_find_extra <- function(root) {

  # if we don't have a root, we don't have a project
  if (is.null(root))
    return(NULL)

  # only run for root-level dependency checks
  project <- renv_project_resolve()
  if (!renv_path_same(root, project))
    return(NULL)

  # only run if we have a custom profile
  profile <- renv_profile_get()
  if (is.null(profile))
    return(NULL)

  # look for dependencies in the associated 'renv' folder
  path <- renv_paths_renv(project = project)
  renv_dependencies_find_impl(path, root, 0L)

}

renv_dependencies_find <- function(path = getwd(), root = getwd()) {
  files <- lapply(path, renv_dependencies_find_impl, root = root, depth = 0)
  extra <- renv_dependencies_find_extra(root)
  unlist(c(files, extra), recursive = TRUE, use.names = FALSE)
}

renv_dependencies_find_impl <- function(path, root, depth) {

  # check file type
  info <- renv_file_info(path)

  # the file might have been removed after listing -- if so, just ignore it
  if (is.na(info$isdir))
    return(NULL)

  # if this is a directory, recurse
  if (info$isdir)
    return(renv_dependencies_find_dir(path, root, depth))

  # otherwise, check and see if we have a registered callback
  callback <- renv_dependencies_callback(path)
  if (is.function(callback))
    return(path)

}

renv_dependencies_find_dir <- function(path, root, depth) {

  # check if this path should be ignored
  excluded <- renv_renvignore_exec(path, root, path)
  if (excluded)
    return(character())

  # check if we've already scanned this directory
  # (necessary to guard against recursive symlinks)
  if (!renv_platform_windows()) {
    norm <- renv_path_normalize(path, mustWork = FALSE)
    state <- renv_dependencies_state()
    if (visited(norm, state$scanned))
      return(character())
  }

  # list children
  children <- renv_dependencies_find_dir_children(path, root, depth)

  # find recursive dependencies
  depth <- depth + 1
  paths <- map(children, renv_dependencies_find_impl, root = root, depth = depth)

  # explicitly include rsconnect folder
  # (so we can infer a dependency on rsconnect when appropriate)
  rsconnect <- file.path(path, "rsconnect")
  if (file.exists(rsconnect))
    paths <- c(rsconnect, paths)

  paths

}

# return the set of files / subdirectories within a directory that should be
# crawled for dependencies
renv_dependencies_find_dir_children <- function(path, root, depth) {

  # list files in the folder
  children <- renv_file_list(path, full.names = TRUE)

  # remove files which are broken symlinks
  children <- children[file.exists(children)]

  # remove hard-coded ignores
  # (only keep DESCRIPTION files at the top level)
  ignored <- c("renv", "packrat", "revdep", if (depth) "DESCRIPTION")
  children <- children[!basename(children) %in% ignored]

  # compute exclusions
  excluded <- renv_renvignore_exec(path, root, children)

  # keep only non-excluded children
  children[!excluded]

}

renv_dependencies_discover <- function(paths, progress, errors) {

  if (!renv_dependencies_discover_preflight(paths, errors))
    return(invisible(NULL))

  # short path if we're not showing progress
  if (identical(progress, FALSE))
    return(bapply(paths, renv_dependencies_discover_impl))

  # otherwise, run with progress reporting

  # nocov start
  vprintf("Finding R package dependencies ... ")
  discover <- renv_progress(renv_dependencies_discover_impl, length(paths))
  deps <- lapply(paths, discover)
  vwritef("Done!")

  bind(deps)
  # nocov end

}

renv_dependencies_discover_impl <- function(path) {

  callback <- renv_dependencies_callback(path)
  if (is.null(callback)) {
    fmt <- "internal error: no callback registered for file %s"
    warningf(fmt, renv_path_pretty(path))
  }

  result <- tryCatch(callback(path), error = warning)
  if (inherits(result, "condition"))
    return(NULL)

  result

}

renv_dependencies_discover_preflight <- function(paths, errors) {

  if (identical(errors, "ignored"))
    return(TRUE)

  # TODO: worth customizing?
  limit <- 1000L
  if (length(paths) < limit)
    return(TRUE)

  lines <- c(
    "A large number of files (%i in total) have been discovered.",
    "It may take renv a long time to crawl these files for dependencies.",
    "Consider using .renvignore to ignore irrelevant files.",
    "See `?dependencies` for more information.",
    "Set `options(renv.config.dependencies.limit = Inf)` to disable this warning.",
    ""
  )
  vwritef(lines, length(paths))

  if (identical(errors, "reported"))
    return(TRUE)

  if (interactive() && !proceed()) {
    renv_report_user_cancel()
    return(FALSE)
  }

  TRUE

}

renv_dependencies_discover_renv_lock <- function(path) {
  renv_dependencies_list(path, "renv")
}

renv_dependencies_discover_description <- function(path,
                                                   fields = NULL,
                                                   subdir = NULL)
{
  dcf <- catch(renv_description_read(path = path, subdir = subdir))
  if (inherits(dcf, "error"))
    return(renv_dependencies_error(path, error = dcf))

  # read fields from project options if available
  fields <-
    fields %||%
    renv_dependencies_discover_description_fields() %||%
    c("Depends", "Imports", "LinkingTo")

  # build pattern used to split DESCRIPTION fields
  pattern <- paste0(
    "([a-zA-Z0-9._]+)",                      # package name
    "(?:\\s*\\(([><=]+)\\s*([0-9.-]+)\\))?"  # optional version specification
  )

  # if this is the DESCRIPTION file for the active project, include
  # Suggests since they're often needed as well. such packages will be
  # considered development dependencies, except for package projects
  state <- renv_dependencies_state()
  type <- "unknown"
  if (identical(file.path(state$root, "DESCRIPTION"), path)) {

    # collect profile-specific dependencies as well
    profile <- renv_profile_get()
    field <- if (length(profile))
      sprintf("Config/renv/profiles/%s/dependencies", profile)

    fields <- c(fields, "Suggests", field)
    type <- renv_description_type(desc = dcf)

  }

  data <- lapply(fields, function(field) {

    # read field
    contents <- dcf[[field]]
    if (!is.character(contents))
      return(list())

    # split on commas
    parts <- strsplit(dcf[[field]], "\\s*,\\s*")[[1]]

    # drop any empty fields
    x <- parts[nzchar(parts)]

    # match to split on package name, version
    m <- regexec(pattern, x)
    matches <- regmatches(x, m)
    if (empty(matches))
      return(list())

    # create dependency list
    dev <- field == "Suggests" && type != "package"
    renv_dependencies_list(
      path,
      extract_chr(matches, 2L),
      extract_chr(matches, 3L),
      extract_chr(matches, 4L),
      dev
    )

  })

  bind(data)

}

renv_dependencies_discover_description_fields <- function() {

  # this is all very gross -- the project should be passed
  # along by the caller instead
  project <- NULL

  # are we being called as part of renv::dependencies()?
  # if so, then use the root directory as the project root
  state <- renv_dependencies_state()
  if (!is.null(state))
    project <- state$root

  # are we being called as part of renv::restore()?
  # if so, then use the associated project directory
  state <- renv_restore_state()
  if (!is.null(state))
    project <- state$project

  # all else fails, use the active project
  project <- project %||% renv_project()
  renv::settings$package.dependency.fields(project = project)

}

renv_dependencies_discover_bookdown <- function(path) {
  # TODO: other dependencies to parse from bookdown?
  renv_dependencies_list(path, "bookdown")
}

renv_dependencies_discover_pkgdown <- function(path) {
  # TODO: other dependencies to parse from pkgdown?
  renv_dependencies_list(path, "pkgdown")
}

renv_dependencies_discover_quarto <- function(path) {
  # TODO: other dependencies to parse from quarto?
  renv_dependencies_list(path, "quarto")
}

renv_dependencies_discover_rsconnect <- function(path) {
  renv_dependencies_list(path, "rsconnect")
}

renv_dependencies_discover_multimode <- function(path, mode) {

  # TODO: find in-line R code?
  deps <- stack()

  if (mode %in% c("rmd", "qmd"))
    deps$push(renv_dependencies_discover_rmd_yaml_header(path, mode))

  deps$push(renv_dependencies_discover_chunks(path, mode))

  bind(Filter(NROW, deps$data()))

}

renv_dependencies_discover_rmd_yaml_header <- function(path, mode) {

  deps <- stack(mode = "character")

  # R Markdown documents always depend on rmarkdown
  if (identical(mode, "rmd"))
    deps$push("rmarkdown")

  # try and read the document's YAML header
  contents <- renv_file_read(path)
  pattern <- "(?:^|\n)\\s*---\\s*(?:$|\n)"
  matches <- gregexpr(pattern, contents, perl = TRUE)[[1L]]

  # check that we have something that looks like a YAML header
  ok <- length(matches) > 1L && matches[[1L]] == 1L
  if (!ok)
    return(renv_dependencies_list(path, packages = deps$data()))

  # require yaml package for parsing YAML header
  name <- case(
    mode == "rmd" ~ "R Markdown",
    mode == "qmd" ~ "Quarto Markdown"
  )

  # validate that we actually have the yaml package available
  if (!renv_dependencies_require("yaml", name)) {
    packages <- deps$data()
    return(renv_dependencies_list(path, packages))
  }

  # extract YAML text
  yamltext <- substring(contents, matches[[1L]] + 4L, matches[[2L]] - 1L)
  yaml <- catch(renv_yaml_load(yamltext))
  if (inherits(yaml, "error"))
    return(renv_dependencies_error(path, error = yaml, packages = "rmarkdown"))

  # check for Shiny runtime
  runtime <- yaml[["runtime"]] %||% ""
  if (pstring(runtime) && grepl("shiny", runtime, fixed = TRUE))
    deps$push("shiny")

  server <- yaml[["server"]] %||% ""
  if (identical(server, "shiny"))
    deps$push("shiny")

  if (is.list(server) && identical(server[["type"]], "shiny"))
    deps$push("shiny")

  pattern <- renv_regexps_package_name()

  # check recursively for package usages of the form 'package::method'
  recurse(yaml, function(node, stack) {

    # look for keys of the form 'package::method'
    values <- c(names(node), if (pstring(node)) node)
    for (value in values) {
      parts <- strsplit(value, ":{2,3}")[[1L]]
      if (length(parts) == 2L) {
        name <- parts[[1L]]
        if (grepl(pattern, name))
          deps$push(parts[[1L]])
      }
    }

  })

  # check for dependency on bslib
  theme <- catchall(yaml[[c("output", "html_document", "theme")]])
  if (!inherits(theme, "error") && is.list(theme))
    deps$push("bslib")

  # check for parameterized documents
  status <- catch(renv_dependencies_discover_rmd_yaml_header_params(yaml, deps))
  if (inherits(status, "error"))
    renv_error_report(status)

  # get list of dependencies
  packages <- deps$data()
  renv_dependencies_list(path, packages)

}

renv_dependencies_discover_rmd_yaml_header_params <- function(yaml, deps) {

  # check for declared params
  params <- yaml[["params"]]
  if (!is.list(params))
    return()

  # infer dependency on shiny
  deps$push("shiny")

  # iterate through params, parsing dependencies from R code
  for (param in params) {

    # check for r types
    type <- attr(param, "type", exact = TRUE)
    if (!identical(type, "r"))
      next

    # attempt to parse dependencies
    rdeps <- catch(renv_dependencies_discover_r(text = param))
    if (inherits(rdeps, "error"))
      next

    # add each dependency
    for (package in sort(unique(rdeps$Package)))
      deps$push(package)

  }

}

renv_dependencies_discover_chunks_ignore <- function(chunk) {

  # if renv.ignore is set, respect it
  ignore <- chunk$params[["renv.ignore"]]
  if (!is.null(ignore))
    return(truthy(ignore))

  # skip non-R chunks
  engine <- chunk$params[["engine"]]
  ok <- is.character(engine) && engine %in% c("r", "rscript")
  if (!ok)
    return(TRUE)

  # skip un-evaluated chunks
  if (!truthy(chunk$params[["eval"]], default = TRUE))
    return(TRUE)

  # skip learnr exercises
  if (truthy(chunk$params[["exercise"]], default = FALSE))
    return(TRUE)

  # skip chunks whose labels end in '-display'
  label <- chunk$params[["label"]] %||% ""
  if (grepl("-display$", label))
    return(TRUE)

  # ok, don't ignore this chunk
  FALSE

}

renv_dependencies_discover_chunks <- function(path, mode) {

  # figure out the appropriate begin, end patterns
  type <- tolower(tools::file_ext(path))
  if (type %in% c("rmd", "qmd", "rmarkdown"))
    type <- "md"

  allpatterns <- renv_knitr_patterns()
  patterns <- allpatterns[[type]]
  if (is.null(patterns)) {
    condition <- simpleCondition("not a recognized multi-mode R document")
    return(renv_dependencies_error(path, error = condition))
  }

  # parse the chunks within
  # NOTE: we need to proceed line-by-line since the chunk end pattern might
  # end chunks not started by the chunk begin pattern (sad face)
  encoding <- if (type == "md") "UTF-8" else "unknown"
  contents <- readLines(path, warn = FALSE, encoding = encoding)
  ranges <- renv_dependencies_discover_chunks_ranges(path, contents, patterns)

  # extract chunk code from the used ranges
  chunks <- .mapply(function(lhs, rhs) {

    # parse params in header
    header <- contents[[lhs]]
    params <- renv_dependencies_discover_parse_params(header, type)

    # extract chunk contents (preserve newlines for nicer error reporting)
    range <- seq.int(lhs + 1, length.out = rhs - lhs - 1)
    code <- rep.int("", length(contents))
    code[range] <- contents[range]

    # return list of outputs
    list(params = params, code = code)

  }, ranges, NULL)

  # iterate over chunks, and attempt to parse dependencies from each
  cdeps <- bapply(chunks, function(chunk) {

    # check whether this chunk should be ignored
    if (renv_dependencies_discover_chunks_ignore(chunk))
      return(character())

    # remove reused chunk placeholders
    pattern <- "<<[^>]+>>"
    code <- gsub(pattern, "", chunk$code)

    # okay, now we can discover deps
    deps <- catch(renv_dependencies_discover_r(path = path, text = code))
    if (inherits(deps, "error"))
      return(renv_dependencies_error(path, error = deps))

    deps

  })

  # check for dependencies in inline chunks as well
  ideps <- renv_dependencies_discover_chunks_inline(path, contents)

  # if this is a .qmd, infer a dependency on rmarkdown if we have any R chunks
  qdeps <- NULL
  if (mode %in% "qmd") {
    for (chunk in chunks) {
      engine <- chunk$params[["engine"]]
      if (is.character(engine) && engine %in% c("r", "rscript")) {
        qdeps <- renv_dependencies_list(path, "rmarkdown")
        break
      }
    }
  }

  # paste them all together
  deps <- bind(list(cdeps, ideps, qdeps))
  if (is.null(deps))
    return(deps)

  deps$Source <- path
  deps

}

renv_dependencies_discover_chunks_inline <- function(path, contents) {

  pasted <- paste(contents, collapse = "\n")
  matches <- gregexpr("`r ([^`]+)`", pasted)
  if (identical(c(matches[[1L]]), -1L))
    return(list())

  text <- unlist(regmatches(pasted, matches), use.names = FALSE, recursive = FALSE)
  code <- substring(text, 4L, nchar(text) - 1L)
  deps <- renv_dependencies_discover_r(path = path, text = code)
  if (inherits(deps, "error"))
    return(renv_dependencies_error(path, error = deps))

  deps

}

renv_dependencies_discover_chunks_ranges <- function(path, contents, patterns) {

  output <- list()

  chunk <- FALSE
  start <- 1; end <- 1
  for (i in seq_along(contents)) {

    line <- contents[[i]]

    if (chunk == FALSE && grepl(patterns$chunk.begin, line)) {
      chunk <- TRUE
      start <- i
      next
    }

    if (chunk == TRUE && grepl(patterns$chunk.begin, line)) {
      end <- i
      output[[length(output) + 1]] <- list(lhs = start, rhs = end)
      start <- i
      next
    }

    if (chunk == TRUE && grepl(patterns$chunk.end, line)) {
      chunk <- FALSE
      end <- i
      output[[length(output) + 1]] <- list(lhs = start, rhs = end)
      next
    }

  }

  if (chunk) {
    message <- sprintf("chunk starting on line %i is not closed", start)
    error <- simpleError(message)
    renv_dependencies_error(path, error = error)
  }

  bind(output)

}

renv_dependencies_discover_rproj <- function(path) {

  props <- renv_properties_read(path)

  deps <- stack()
  if (identical(props$PackageUseDevtools, "Yes")) {
    deps$push("devtools")
    deps$push("roxygen2")
  }

  renv_dependencies_list(path, deps$data(), dev = TRUE)

}

renv_dependencies_discover_r <- function(path = NULL,
                                         text = NULL,
                                         expr = NULL,
                                         envir = NULL)
{
  expr <- case(
    is.function(expr)  ~ body(expr),
    is.language(expr)  ~ expr,
    is.character(expr) ~ catch(renv_parse_text(expr)),
    is.character(text) ~ catch(renv_parse_text(text)),
    is.character(path) ~ catch(renv_parse_file(path)),
    ~ stop("internal error")
  )

  if (inherits(expr, "error"))
    return(renv_dependencies_error(path, error = expr))

  # update current path
  state <- renv_dependencies_state()
  if (!is.null(state))
    renv_scope_var("path", path, envir = state)

  methods <- c(
    renv_dependencies_discover_r_methods,
    renv_dependencies_discover_r_xfun,
    renv_dependencies_discover_r_library_require,
    renv_dependencies_discover_r_require_namespace,
    renv_dependencies_discover_r_colon,
    renv_dependencies_discover_r_pacman,
    renv_dependencies_discover_r_modules,
    renv_dependencies_discover_r_import,
    renv_dependencies_discover_r_box,
    renv_dependencies_discover_r_targets,
    renv_dependencies_discover_r_glue,
    renv_dependencies_discover_r_parsnip,
    renv_dependencies_discover_r_database
  )

  envir <- envir %||% new.env(parent = emptyenv())
  recurse(expr, function(node, stack) {

    # normalize calls (handle magrittr pipes)
    node <- renv_call_normalize(node, stack)

    # invoke methods on call objects
    if (is.call(node))
      for (method in methods)
        method(node, stack, envir)

    # return node
    node

  })

  packages <- ls(envir = envir, all.names = TRUE)
  renv_dependencies_list(path, packages)
}

renv_dependencies_discover_r_methods <- function(node, stack, envir) {

  node <- renv_call_expect(node, "methods", c("setClass", "setGeneric"))
  if (is.null(node))
    return(FALSE)

  envir[["methods"]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_xfun <- function(node, stack, envir) {

  node <- renv_call_expect(node, "xfun", c("pkg_attach", "pkg_attach2"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  prototype <- function(..., install = FALSE, message = TRUE) {}
  matched <- catch(match.call(prototype, node, expand.dots = FALSE))
  if (inherits(matched, "error"))
    return(FALSE)

  # extract character vectors from `...`
  strings <- stack()
  recurse(matched[["..."]], function(node, stack) {
    if (is.character(node))
      strings$push(node)
  })

  # mark packages as known
  packages <- strings$data()
  if (empty(packages))
    return(FALSE)

  for (package in packages)
    envir[[package]] <- TRUE

  TRUE
}

renv_dependencies_discover_r_library_require <- function(node, stack, envir) {

  node <- renv_call_expect(node, "base", c("library", "require"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  matched <- catch(match.call(base::library, node))
  if (inherits(matched, "error"))
    return(FALSE)

  # if the 'package' argument is a character vector of length one, we're done
  if (is.character(matched$package) &&
      length(matched$package) == 1)
  {
    envir[[matched$package]] <- TRUE
    return(TRUE)
  }

  # if it's a symbol, double check character.only argument
  if (is.symbol(matched$package) &&
      identical(matched$character.only %||% FALSE, FALSE))
  {
    envir[[as.character(matched$package)]] <- TRUE
    return(TRUE)
  }

  FALSE

}

renv_dependencies_discover_r_require_namespace <- function(node, stack, envir) {

  node <- renv_call_expect(node, "base", c("requireNamespace", "loadNamespace"))
  if (is.null(node))
    return(FALSE)

  f <- get(as.character(node[[1]]), envir = .BaseNamespaceEnv, inherits = FALSE)
  matched <- catch(match.call(f, node))
  if (inherits(matched, "error"))
    return(FALSE)

  package <- matched$package
  if (is.character(package) && length(package == 1)) {
    envir[[package]] <- TRUE
    return(TRUE)
  }

  FALSE


}

renv_dependencies_discover_r_colon <- function(node, stack, envir) {

  ok <-
    is.call(node) &&
    length(node) == 3 &&
    is.name(node[[1]]) &&
    as.character(node[[1]]) %in% c("::", ":::")

  if (!ok)
    return(FALSE)

  package <- node[[2L]]
  if (is.symbol(package))
    package <- as.character(package)

  if (!is.character(package) || length(package) != 1)
    return(FALSE)

  envir[[package]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_pacman <- function(node, stack, envir) {

  node <- renv_call_expect(node, "pacman", "p_load")
  if (is.null(node) || length(node) < 2)
    return(FALSE)

  # check for character.only
  chonly <- node[["character.only"]] %||% FALSE

  # consider all unnamed arguments
  parts <- as.list(node[-1L])

  # consider packages passed to 'char' parameter
  char <- node[["char"]]

  # detect vector of packages passed as vector
  if (is.call(char) && identical(char[[1L]], as.name("c")))
    parts <- c(parts, as.list(char[-1L]))

  # detect plain old package name
  if (is.character(char))
    parts <- c(parts, as.list(char))

  # ensure names
  names(parts) <- names(parts) %||% rep.int("", length(parts))
  unnamed <- parts[!nzchar(names(parts))]

  # extract symbols / characters
  for (arg in unnamed) {

    # skip symbols if necessary
    if (chonly && is.symbol(arg))
      next

    # check for character or symbol
    ok <-
      length(arg) == 1 &&
      is.character(arg) ||
      is.symbol(arg)

    if (!ok)
      next

    # add it
    envir[[as.character(arg)]] <- TRUE

  }

  TRUE

}

renv_dependencies_discover_r_modules <- function(node, stack, envir) {

  # check for call of the form 'pkg::foo(a, b, c)'
  colon <-
    is.call(node[[1L]]) &&
    is.name(node[[1L]][[1L]]) &&
    as.character(node[[1L]][[1L]]) %in% c("::", ":::")

  node <- renv_call_expect(node, "modules", c("import"))
  if (is.null(node))
    return(FALSE)

  ok <- FALSE
  if (colon) {
    # include if fully qualified call to modules::import
    ok <- TRUE
  } else {
    # otherwise only consider calls within a 'module' block
    # (to reduce confusion with reticulate::import)
    for (parent in stack) {
      parent <- renv_call_expect(parent, "modules", c("amodule", "module"))
      if (!is.null(parent)) {
        ok <- TRUE
        break
      }
    }
  }

  if (!ok)
    return(FALSE)

  # attempt to match the call
  prototype <- function(from, ..., attach = TRUE, where = parent.frame()) {}
  matched <- catch(match.call(prototype, node, expand.dots = FALSE))
  if (inherits(matched, "error"))
    return(FALSE)

  # extract character vector or symbol from `from`
  package <- matched[["from"]]
  if (empty(package))
    return(FALSE)

  # package could be symbols or character so call as.character
  # to be safe then mark packages as known
  envir[[as.character(package)]] <- TRUE

  TRUE
}

renv_dependencies_discover_r_import <- function(node, stack, envir) {

  node <- renv_call_expect(node, "import", c("from", "here", "into"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  name <- as.character(node[[1L]])
  matched <- if (name == "from") {
    catch(match.call(function(.from, ...) {}, node, expand.dots = FALSE))
  } else {
    catch(match.call(function(..., .from) {}, node, expand.dots = FALSE))
  }

  if (inherits(matched, "error"))
    return(FALSE)

  # the '.from' argument is the package name, either a character vector of length one or a symbol
  from <- matched$.from
  if (is.symbol(from))
    from <- as.character(from)

  ok <-
    is.character(from) &&
    length(from) == 1

  if (!ok)
    return(FALSE)

  envir[[from]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_box <- function(node, stack, envir) {

  node <- renv_call_expect(node, "box", "use")
  if (is.null(node))
    return(FALSE)

  for (i in seq.int(2L, length.out = length(node) - 1L))
    renv_dependencies_discover_r_box_impl(node[[i]], stack, envir)

  TRUE

}

renv_dependencies_discover_r_box_impl <- function(node, stack, envir) {

  # if the node is just a symbol, then it's the name of a package
  # otherwise, if it's a call to `[`, the first argument is the package name
  name <- if (is.symbol(node) && ! identical(node, quote(expr = ))) {
    as.character(node)
  } else if (
    is.call(node) &&
      length(node) > 1L &&
      identical(node[[1L]], as.name("[")) &&
      is.symbol(node[[2L]])) {
    as.character(node[[2L]])
  }

  # the names `.` and `..` are special place holders and don't refer to packages
  if (is.null(name) || name == "." || name == "..")
    return(FALSE)

  envir[[name]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_targets <- function(node, stack, envir) {

  node <- renv_call_expect(node, "targets", "tar_option_set")
  if (is.null(node))
    return(FALSE)

  envir[["targets"]] <- TRUE

  packages <- tryCatch(
    renv_dependencies_eval(node$packages),
    error = identity
  )

  # TODO: evaluation can fail for a multitude of reasons;
  # are any of these worth signalling to the user?
  if (inherits(packages, "error"))
    return(TRUE)

  if (is.character(packages))
    for (package in packages)
      envir[[package]] <- TRUE

  TRUE

}

renv_dependencies_discover_r_glue <- function(node, stack, envir) {

  node <- renv_call_expect(node, "glue", "glue")
  if (is.null(node))
    return(FALSE)

  # analyze all unnamed strings in the call
  args <- as.list(node)[-1L]
  nm <- names(args) %||% rep.int("", length(args))
  strings <- args[!nzchar(nm) & map_lgl(args, is.character)]

  # start iterating through the strings, looking for code chunks
  for (string in strings)
    renv_dependencies_discover_r_glue_impl(string, node, envir)

  TRUE

}

renv_dependencies_discover_r_glue_impl <- function(string, node, envir) {

  # get open, close delimiters
  ropen    <- charToRaw(node$.open    %||% "{")
  rclose   <- charToRaw(node$.close   %||% "}")
  rcomment <- charToRaw(node$.comment %||% "#")

  # constants
  rcomment   <- charToRaw("#")
  rbackslash <- charToRaw("\\")
  rquotes <- c(
    charToRaw("'"),
    charToRaw("\""),
    charToRaw("`")
  )

  # iterate through characters in string
  raw <- c(charToRaw(string), as.raw(0L))
  i <- 0L
  n <- length(raw)
  quote <- raw()

  # index for open delimiter match
  index <- 0L
  count <- 0L

  while (i < n) {

    # ensure we always advance index
    i <- i + 1L

    # handle quoted states
    if (length(quote)) {

      # skip escaped characters
      if (raw[[i]] == rbackslash) {
        i <- i + 1L
        next
      }

      # check for escape from quote
      if (raw[[i]] == quote) {
        quote <- raw()
        next
      }

    }

    # skip comments
    if (raw[[i]] == rcomment) {
      i <- grepRaw("(?:$|\n)", raw, i)
      next
    }

    # skip escaped characters
    if (raw[[i]] == rbackslash) {
      i <- i + 1L
      next
    }

    # check for quotes
    idx <- match(raw[[i]], rquotes, nomatch = 0L)
    if (idx > 0) {
      quote <- rquotes[[idx]]
      next
    }

    # check for open delimiter
    if (i %in% grepRaw(ropen, raw, i, fixed = TRUE)) {

      # check for duplicate (escape)
      j <- i + length(ropen)
      if (j %in% grepRaw(ropen, raw, j, fixed = TRUE)) {
        i <- j + length(ropen) - 1L
        next
      }

      # save index if we're starting a match
      if (count == 0L) {
        index <- i
      }

      # increment match count
      count <- count + 1L
      next

    }

    # check for close delimiter
    if (i %in% grepRaw(rclose, raw, i, fixed = TRUE)) {

      # check for duplicate (escape)
      j <- i + length(rclose)
      if (j %in% grepRaw(rclose, raw, j, fixed = TRUE)) {
        i <- j + length(rclose) - 1L
        next
      }

      if (count > 0L) {

        # decrement count if we have a match
        count <- count - 1L

        # check for match and parse dependencies within
        if (count == 0L) {

          # extract inner code
          lhs <- index + length(ropen)
          rhs <- i - 1L
          code <- rawToChar(raw[lhs:rhs])

          # parse dependencies
          deps <- renv_dependencies_discover_r(text = code, envir = envir)

        }

      }

    }

  }

}

renv_dependencies_discover_r_parsnip <- function(node, stack, envir) {

  node <- renv_call_expect(node, "parsnip", "set_engine")
  if (is.null(node))
    return(FALSE)

  matched <- catch(match.call(function(object, engine, ...) {}, node))
  if (inherits(matched, "error"))
    return(FALSE)

  engine <- matched$engine
  if (!is.character(engine) || length(engine) != 1L)
    return(FALSE)

  map <- getOption("renv.parsnip.engines", default = list(
    glm    = "stats",
    glmnet = "glmnet",
    keras  = "keras",
    kknn   = "kknn",
    nnet   = "nnet",
    rpart  = "rpart",
    spark  = "sparklyr",
    stan   = "rstanarm"
  ))

  packages <- if (is.function(map))
    tryCatch(map(engine), error = function(e) NULL)
  else
    map[[engine]]

  if (is.null(packages))
    return(FALSE)

  for (package in packages)
    envir[[package]] <- TRUE

  # TODO: a number of model routines appear to depend on dials;
  # should we just assume it's required by default? or should
  # users normally be using tidymodels instead of parsnip directly?
  TRUE

}

renv_dependencies_discover_r_database <- function(node, stack, envir) {

  found <- FALSE

  db <- renv_dependencies_database()
  enumerate(db, function(package, dependencies) {
    enumerate(dependencies, function(method, requirements) {

      expect <- renv_call_expect(node, package, method)
      if (is.null(expect))
        return(FALSE)

      for (requirement in requirements)
        envir[[requirement]] <- TRUE

      found <<- TRUE
      TRUE

    })
  })

  found

}

renv_dependencies_database <- function() {
  # TODO: make this user-accessible?
  renv_global(
    "dependencies.database",
    renv_dependencies_database_impl()
  )
}

renv_dependencies_database_impl <- function() {

  db <- list()

  db$ggplot2 <- list(
    geom_hex = "hexbin"
  )

  db

}

renv_dependencies_list <- function(source,
                                   packages,
                                   require = "",
                                   version = "",
                                   dev = FALSE)
{
  if (empty(packages))
    return(renv_dependencies_list_empty())

  source <- source %||% rep.int(NA_character_, length(packages))

  data.frame(
    Source  = as.character(source),
    Package = as.character(packages),
    Require = require,
    Version = version,
    Dev     = dev,
    stringsAsFactors = FALSE
  )

}

renv_dependencies_list_empty <- function() {

  data.frame(
    Source  = character(),
    Package = character(),
    Require = character(),
    Version = character(),
    Dev     = logical(),
    stringsAsFactors = FALSE
  )

}


renv_dependencies_discover_parse_params <- function(header, type) {

  engine <- "r"
  patterns <- renv_knitr_patterns()
  rest <- sub(patterns[[type]]$chunk.begin, "\\1", header)

  # if this is an R Markdown document, parse the initial engine chunk
  if (type == "md") {
    idx <- regexpr("(?:[ ,]|$)", rest)
    engine <- substring(rest, 1, idx - 1)
    rest <- sub("^,*\\s*", "", substring(rest, idx + 1))
  }

  # extract an unquoted label
  label <- ""
  pattern <- "(^\\s*[^=]+)(,|\\s*$)"
  matches <- regexec(pattern, rest)[[1]]
  if (!identical(c(matches), -1L)) {
    submatches <- regmatches(rest, list(matches))[[1]]
    label <- trimws(submatches[[2L]])
    rest <- substring(rest, matches[[3L]] + 1L)
  }

  params <- catch(parse(text = sprintf("alist(%s)", rest))[[1]])
  if (inherits(params, "error"))
    return(list(engine = engine))

  # inject the label back in
  names(params) <- names(params) %||% rep.int("", length(params))
  if (length(params) > 1 && names(params)[[2L]] == "")
    names(params)[[2L]] <- "label"

  # fix up 'label' if it's a missing value
  if (identical(params[["label"]], quote(expr = )))
    params[["label"]] <- NULL

  if (is.null(params[["label"]]) && nzchar(label))
    params[["label"]] <- label

  if (is.null(params[["engine"]]))
    params[["engine"]] <- engine

  eval(params, envir = parent.frame())

}

renv_dependencies_require <- function(package, type) {

  if (requireNamespace(package, quietly = TRUE))
    return(TRUE)

  fmt <- lines(
    "The '%1$s' package is required to parse dependencies within %2$s files.",
    "Consider installing it with `install.packages(\"%1$s\")`."
  )
  msg <- sprintf(fmt, package, type)

  if (renv_once())
    warning(msg, call. = FALSE)

  return(FALSE)

}

renv_dependencies_state <- function() {
  renv_global_get("dependencies.state")
}

renv_dependencies_begin <- function(root = NULL) {
  state <- env(root = root, scanned = env(), problems = stack())
  renv_global_set("dependencies.state", state)
}

renv_dependencies_end <- function() {
  renv_global_clear("dependencies.state")
}

renv_dependencies_error <- function(path, error = NULL, packages = NULL) {

  # if no error, return early
  if (is.null(error))
    return(renv_dependencies_list(path, packages))

  # check for missing state (e.g. if internal method called directly)
  state <- renv_dependencies_state()
  if (!is.null(state)) {
    path <- path %||% state$path
    problem <- list(file = path, error = error)
    state$problems$push(problem)
  }

  # return dependency list
  renv_dependencies_list(path, packages)

}

renv_dependencies_report <- function(errors) {

  if (identical(errors, "ignored"))
    return(FALSE)

  state <- renv_dependencies_state()
  if (is.null(state))
    return(FALSE)

  problems <- state$problems$data()
  if (empty(problems))
    return(TRUE)

  ewritef("WARNING: One or more problems were discovered while enumerating dependencies.\n")

  # bind into list
  bound <- bapply(problems, function(problem) {
    fields <- c(aliased_path(problem$file), problem$line, problem$column)
    header <- paste(fields, collapse = ":")
    message <- conditionMessage(problem$error)
    c(file = problem$file, header = header, message = message)
  })

  # split based on header (group errors from same file)
  splat <- split(bound, bound$file)

  # emit messages
  enumerate(splat, function(file, problem) {
    lines <- paste(rep.int("-", nchar(file)), collapse = "")
    prefix <- format(paste("ERROR", seq_along(problem$message)))
    messages <- paste(prefix, problem$message, sep = ": ", collapse = "\n\n")
    text <- c(file, lines, "", messages, "")
    ewritef(text)
  })

  ewritef("Please see `?renv::dependencies` for more information.")

  if (identical(errors, "fatal")) {
    fmt <- "one or more errors occurred while enumerating dependencies"
    stopf(fmt)
  }

  renv_condition_signal("renv.dependencies.error", problems)
  TRUE

}

renv_dependencies_scope <- function(path, action, .envir = NULL) {

  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)
  if (exists(path, envir = `_renv_dependencies`))
    return(get(path, envir = `_renv_dependencies`))

  errors <- config$dependency.errors()
  message <- paste(action, "aborted")

  deps <- withCallingHandlers(
    dependencies(path, progress = FALSE, errors = errors, dev = TRUE),
    renv.dependencies.error = renv_dependencies_error_handler(message, errors)
  )

  assign(path, deps, envir = `_renv_dependencies`)

  envir <- .envir %||% parent.frame()
  defer(rm(list = path, envir = `_renv_dependencies`), envir = envir)

  invisible(deps)

}

renv_dependencies_error_handler <- function(message, errors) {

  force(message)
  force(errors)

  function(condition) {

    if (identical(errors, "fatal") || interactive() && !proceed()) {

      condition <- structure(
        list(message = message, call = NULL, traceback = FALSE),
        class = c("renv.dependencies.error", "error", "condition")
      )

      stop(condition)

    }

    renv_condition_data(condition)

  }
}

renv_dependencies_eval <- function(expr) {

  # create environment with small subset of symbols
  syms <- c("list", "c")
  vals <- mget(syms, envir = baseenv())
  envir <- list2env(vals, parent = emptyenv())

  # evaluate in that environment
  eval(expr, envir = envir)

}
