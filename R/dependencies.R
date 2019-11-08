
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
#' @param path The path to a (possibly multi-mode) \R file, or a directory
#'   containing such files.
#'
#' @param root The root directory to be used for dependency discovery.
#'   Defaults to the active project directory. You may need to set this
#'   explicitly to ensure that your project's `.renvignore`s (if any) are
#'   properly handled.
#'
#' @param quiet Boolean; report problems discovered (if any) during dependency
#'   discovery?
#'
#' @param dev Boolean; include 'development' dependencies as well? That is,
#'   packages which may be required during development but are unlikely to be
#'   required during runtime for your project. By default, only runtime
#'   dependencies are returned.
#'
#' @return An \R `data.frame` of discovered dependencies, mapping inferred
#'   package names to the files in which they were discovered.
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
dependencies <- function(path = getwd(),
                         root = NULL,
                         quiet = FALSE,
                         dev = FALSE)
{
  renv_scope_error_handler()

  renv_dependencies_begin()
  on.exit(renv_dependencies_end(), add = TRUE)

  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)
  root <- root %||% renv_dependencies_root(path)

  files <- renv_dependencies_find(path, root)
  deps <- renv_dependencies_discover(files, quiet)

  if (!quiet)
    renv_dependencies_report()

  if (empty(deps) || nrow(deps) == 0L)
    return(deps)

  if (identical(dev, FALSE))
    deps <- deps[!deps$Dev, ]

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
    "renv.lock"     = function(path) renv_dependencies_discover_renv_lock(path),
    "rsconnect"     = function(path) renv_dependencies_discover_rsconnect(path)
  )

  cbext <- list(
    ".rproj" = function(path) renv_dependencies_discover_rproj(path),
    ".r"     = function(path) renv_dependencies_discover_r(path),
    ".rmd"   = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rnw"   = function(path) renv_dependencies_discover_multimode(path, "rnw")
  )

  cbname[[basename(path)]] %||% cbext[[tolower(fileext(path))]]

}

renv_dependencies_find <- function(path = getwd(), root = getwd()) {
  files <- lapply(path, renv_dependencies_find_impl, root = root)
  unlist(files, recursive = TRUE, use.names = FALSE)
}

renv_dependencies_find_impl <- function(path, root) {

  # check file type
  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("file '%s' does not exist", path)

  if (info$isdir)
    return(renv_dependencies_find_dir(path, root))

  callback <- renv_dependencies_callback(path)
  if (is.function(callback))
    return(path)

}

renv_dependencies_find_dir <- function(path, root) {

  path <- renv_renvignore_exec(path, root, path)
  if (empty(path))
    return(character())

  children <- renv_dependencies_find_dir_children(path, root)
  paths <- lapply(children, renv_dependencies_find_impl, root = root)

  rsconnect <- file.path(path, "rsconnect")
  if (file.exists(rsconnect))
    paths <- c(rsconnect, paths)

  paths

}

# return the set of files / subdirectories within a directory that should be
# crawled for dependencies
renv_dependencies_find_dir_children <- function(path, root) {

  # list files in the folder
  children <- renv_file_list(path, full.names = TRUE)

  # remove files which are broken symlinks
  children <- children[file.exists(children)]

  # remove hard-coded ignores
  ignored <- c("renv")
  children <- children[!basename(children) %in% ignored]

  # construct pattern for matching files in this path
  # (return all files if no such pattern available)
  renv_renvignore_exec(path, root, children)

}

renv_dependencies_discover <- function(paths, quiet) {

  if (!renv_dependencies_discover_preflight(paths, quiet))
    return(invisible(NULL))

  # short path if we're being quiet
  if (quiet || renv_testing()) {
    deps <- lapply(paths, renv_dependencies_discover_impl)
    return(bind_list(deps))
  }

  # otherwise, run with progress reporting

  # nocov start
  vprintf("Finding R package dependencies ... ")
  discover <- renv_progress(renv_dependencies_discover_impl, length(paths))
  deps <- lapply(paths, discover)
  vwritef("Done!")

  bind_list(deps)
  # nocov end

}

renv_dependencies_discover_impl <- function(path) {

  entry <- renv_filebacked_get("dependencies", path)
  if (!is.null(entry))
    return(entry)

  callback <- renv_dependencies_callback(path)
  if (is.null(callback)) {
    fmt <- "internal error: no callback registered for file %s"
    warningf(fmt, shQuote(aliased_path(callback), type = "cmd"))
  }

  result <- tryCatch(callback(path), error = warning)
  if (inherits(result, "condition"))
    return(NULL)

  renv_filebacked_set("dependencies", path, result)
  result

}

renv_dependencies_discover_preflight <- function(paths, quiet) {

  limit <- renv_config("dependencies.limit", default = 1000L)
  if (quiet || length(paths) < limit)
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

  if (interactive() && !proceed()) {
    message("* Operation aborted.")
    return(FALSE)
  }

  TRUE

}

renv_dependencies_discover_renv_lock <- function(path) {
  renv_dependencies_list(path, "renv")
}

renv_dependencies_discover_description <- function(path, fields = NULL) {

  dcf <- catch(renv_description_read(path))
  if (inherits(dcf, "error"))
    return(renv_dependencies_error(path, error = dcf))

  # TODO: make this user-configurable?
  fields <- fields %||% c("Depends", "Imports", "LinkingTo")
  pattern <- "([a-zA-Z0-9._]+)(?:\\s*\\(([><=]+)\\s*([0-9.-]+)\\))?"

  # if this is the DESCRIPTION file for the active project, include
  # Suggests since they're often needed as well
  if (identical(renv_project(), dirname(path)))
    fields <- c(fields, "Suggests")

  data <- lapply(fields, function(field) {

    contents <- dcf[[field]]
    if (!is.character(contents))
      return(list())

    x <- strsplit(dcf[[field]], "\\s*,\\s*")[[1]]
    m <- regexec(pattern, x)
    matches <- regmatches(x, m)
    if (empty(matches))
      return(list())

    dev <- field == "Suggests"
    renv_dependencies_list(
      path,
      extract_chr(matches, 2L),
      extract_chr(matches, 3L),
      extract_chr(matches, 4L),
      dev
    )

  })

  bind_list(data)

}

renv_dependencies_discover_pkgdown <- function(path) {

  # TODO: other dependencies to parse from pkgdown?
  renv_dependencies_list(path, "pkgdown")
}

renv_dependencies_discover_bookdown <- function(path) {

  # TODO: other dependencies to parse from bookdown?
  renv_dependencies_list(path, "bookdown")
}

renv_dependencies_discover_rsconnect <- function(path) {
  renv_dependencies_list(path, "rsconnect")
}

renv_dependencies_discover_multimode <- function(path, mode) {

  # TODO: find in-line R code?
  deps <- stack()

  if (identical(mode, "rmd"))
    deps$push(renv_dependencies_discover_rmd_yaml_header(path))

  deps$push(renv_dependencies_discover_chunks(path))

  bind_list(Filter(NROW, deps$data()))

}

renv_dependencies_discover_rmd_yaml_header <- function(path) {

  for (package in c("rmarkdown", "yaml"))
    if (!renv_dependencies_require(package, "R Markdown"))
      return(renv_dependencies_list(path, packages = "rmarkdown"))

  yaml <- catch(rmarkdown::yaml_front_matter(path))
  if (inherits(yaml, "error"))
    return(renv_dependencies_error(path, error = yaml, packages = "rmarkdown"))

  deps <- stack()
  deps$push("rmarkdown")

  # check for Shiny runtime
  runtime <- yaml$runtime %||% ""
  if (is_string(runtime) && grepl("shiny", runtime, fixed = TRUE))
    deps$push("shiny")

  # check for custom output function from another package
  output <- yaml$output %||% ""
  if (is.list(output) && length(output) == 1)
    output <- names(output)

  if (is_string(output)) {
    splat <- strsplit(output, ":{2,3}")[[1]]
    if (length(splat) == 2)
      deps$push(splat[[1]])
  }

  packages <- as.character(deps$data())
  renv_dependencies_list(path, packages)

}

renv_dependencies_discover_chunks <- function(path) {

  # ensure 'knitr' is installed / available
  if (!renv_dependencies_require("knitr", "multi-mode"))
    return(list())

  # figure out the appropriate begin, end patterns
  type <- tolower(tools::file_ext(path))
  if (type %in% c("rmd", "rmarkdown"))
    type <- "md"

  patterns <- knitr::all_patterns[[type]]
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

    # skip non-R chunks
    engine <- chunk$params$engine
    if (!(identical(engine, "r") || identical(engine, "rscript")))
      return(character())

    # skip un-evaluated chunks
    if (identical(chunk$params$eval, FALSE))
      return(character())

    # skip explicitly-ignored chunks
    if (identical(chunk$params$renv.ignore, TRUE))
      return(character())

    deps <- catch(renv_dependencies_discover_r(path = path, text = chunk$code))
    if (inherits(deps, "error"))
      return(renv_dependencies_error(path, error = deps))

    deps

  })

  # check for dependencies in inline chunks as well
  ideps <- renv_dependencies_discover_chunks_inline(path, contents)

  deps <- bind_list(list(cdeps, ideps))
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

  bind_list(output)

}

renv_dependencies_discover_rproj <- function(path) {

  props <- renv_read_properties(path)

  deps <- stack()
  if (identical(props$PackageUseDevtools, "Yes")) {
    deps$push("devtools")
    deps$push("roxygen2")
  }

  renv_dependencies_list(path, deps$data(), dev = TRUE)

}

renv_dependencies_discover_r <- function(path = NULL, text = NULL) {

  parsed <- catch(renv_parse(file = path, text = text))
  if (inherits(parsed, "error")) {
    # workaround for an R bug where parse-related state could be
    # leaked if an error occurred
    Sys.setlocale()
    return(renv_dependencies_error(path, error = parsed))
  }

  methods <- c(
    renv_dependencies_discover_r_methods,
    renv_dependencies_discover_r_xfun,
    renv_dependencies_discover_r_library_require,
    renv_dependencies_discover_r_require_namespace,
    renv_dependencies_discover_r_colon,
    renv_dependencies_discover_r_pacman
  )

  discoveries <- new.env(parent = emptyenv())
  recurse(parsed, function(node) {
    for (method in methods)
      if (is.call(node))
        method(node, discoveries)
  })

  packages <- ls(envir = discoveries)
  if (empty(packages))
    return(list())

  renv_dependencies_list(path, packages)

}

renv_dependencies_discover_r_methods <- function(node, envir) {

  node <- renv_call_expect(node, "methods", c("setClass", "setGeneric"))
  if (is.null(node))
    return(FALSE)

  envir[["methods"]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_xfun <- function(node, envir) {

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
  recurse(matched[["..."]], function(node) {
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

renv_dependencies_discover_r_library_require <- function(node, envir) {

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

renv_dependencies_discover_r_require_namespace <- function(node, envir) {

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

renv_dependencies_discover_r_colon <- function(node, envir) {

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

renv_dependencies_discover_r_pacman <- function(node, envir) {

  node <- renv_call_expect(node, "pacman", "p_load")
  if (is.null(node) || length(node) < 2)
    return(FALSE)

  # check for character.only
  chonly <- node[["character.only"]] %||% FALSE

  # consider all unnamed arguments
  parts <- as.list(node[-1L])

  # also add in 'char'
  parts <- c(parts, as.list(node[["char"]][-1L]))

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

renv_dependencies_list <- function(source,
                                   packages,
                                   require = "",
                                   version = "",
                                   dev = FALSE)
{
  if (empty(packages))
    return(list())

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

renv_dependencies_discover_parse_params <- function(header, type) {

  engine <- "r"
  rest <- sub(knitr::all_patterns[[type]]$chunk.begin, "\\1", header)

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

renv_dependencies_begin <- function() {
  renv_global_set("dependencies.state", env(problems = stack()))
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
    problem <- list(file = path, error = error)
    state$problems$push(problem)
  }

  # return dependency list
  renv_dependencies_list(path, packages)

}

renv_dependencies_report <- function() {

  state <- renv_dependencies_state()
  if (is.null(state))
    return(FALSE)

  problems <- state$problems$data()
  if (empty(problems))
    return(TRUE)

  vwritef("WARNING: One or more problems were discovered while discovering dependencies.\n", con = stderr())

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
    vwritef(text, con = stderr())
  })

  TRUE

}
