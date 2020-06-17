
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

  deps <- delegate(renv_dependencies_impl)
  if (empty(deps) || nrow(deps) == 0L)
    return(deps)

  if (identical(dev, FALSE))
    deps <- deps[!deps$Dev, ]

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
  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)
  root <- root %||% renv_dependencies_root(path)

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
    "renv.lock"     = function(path) renv_dependencies_discover_renv_lock(path),
    "rsconnect"     = function(path) renv_dependencies_discover_rsconnect(path)
  )

  cbext <- list(
    ".rproj"       = function(path) renv_dependencies_discover_rproj(path),
    ".r"           = function(path) renv_dependencies_discover_r(path),
    ".rmd"         = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rmarkdown"   = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rnw"         = function(path) renv_dependencies_discover_multimode(path, "rnw")
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

  # the file might have been removed after listing -- if so, just ignore it
  if (is.na(info$isdir))
    return(NULL)

  # if this is a directory, recurse
  if (info$isdir)
    return(renv_dependencies_find_dir(path, root))

  # otherwise, check and see if we have a registered callback
  callback <- renv_dependencies_callback(path)
  if (is.function(callback))
    return(path)

}

renv_dependencies_find_dir <- function(path, root) {

  # check if this path should be ignored
  path <- renv_renvignore_exec(path, root, path)
  if (empty(path))
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
  children <- renv_dependencies_find_dir_children(path, root)
  paths <- lapply(children, renv_dependencies_find_impl, root = root)

  # explicitly include rsconnect folder
  # (so we can infer a dependency on rsconnect when appropriate)
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

  bind_list(deps)
  # nocov end

}

renv_dependencies_discover_impl <- function(path) {

  callback <- renv_dependencies_callback(path)
  if (is.null(callback)) {
    fmt <- "internal error: no callback registered for file %s"
    warningf(fmt, shQuote(aliased_path(callback), type = "cmd"))
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

  # read fields from project options if available
  fields <-
    fields %||%
    renv_dependencies_discover_description_fields() %||%
    c("Depends", "Imports", "LinkingTo")

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
    fields <- c(fields, "Suggests")
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

  bind_list(data)

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

  # check for custom site generator function from another package
  output <- yaml$site %||% ""

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

  props <- renv_properties_read(path)

  deps <- stack()
  if (identical(props$PackageUseDevtools, "Yes")) {
    deps$push("devtools")
    deps$push("roxygen2")
  }

  renv_dependencies_list(path, deps$data(), dev = TRUE)

}

renv_dependencies_discover_r <- function(path = NULL, text = NULL) {

  parsed <- if (is.character(text))
    catch(renv_parse_text(text))
  else
    catch(renv_parse_file(path))

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
    renv_dependencies_discover_r_pacman,
    renv_dependencies_discover_r_modules,
    renv_dependencies_discover_r_import,
    renv_dependencies_discover_r_database
  )

  discoveries <- new.env(parent = emptyenv())
  recurse(parsed, function(node, stack) {
    if (is.call(node))
      for (method in methods)
        method(node, stack, discoveries)
  })

  packages <- ls(envir = discoveries)
  if (empty(packages))
    return(list())

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
