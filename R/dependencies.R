
#' Find R Package Dependencies in a Project
#'
#' Find \R packages used within a project. `dependencies()` will crawl files
#' within your project, looking for \R files and the packages used within those
#' \R files. This is done primarily by parsing the code and looking for calls of
#' the form:
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
#' @export
dependencies <- function(path = getwd()) {
  renv_scope_error_handler()
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  renv_dependencies_discover(path, path)
}

renv_dependencies_discover <- function(path = getwd(), root = getwd()) {

  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("file '%s' does not exist", path)

  if (info$isdir)
    return(renv_dependencies_discover_dir(path, root))

  name <- basename(path)
  ext <- tolower(tools::file_ext(path))

  case(

    # special cases for special filenames
    name == "DESCRIPTION"   ~ renv_dependencies_discover_description(path),
    name == "_pkgdown.yml"  ~ renv_dependencies_discover_pkgdown(path),
    name == "_bookdown.yml" ~ renv_dependencies_discover_bookdown(path),

    # generic extension-based lookup
    ext == "rproj" ~ renv_dependencies_discover_rproj(path),
    ext == "r"     ~ renv_dependencies_discover_r(path),
    ext == "rmd"   ~ renv_dependencies_discover_multimode(path, "rmd"),
    ext == "rnw"   ~ renv_dependencies_discover_multimode(path, "rnw")

  )

}

renv_dependencies_discover_dir <- function(path, root) {
  children <- renv_dependencies_discover_dir_children(path, root)
  deps <- lapply(children, renv_dependencies_discover, root = root)
  bind_list(deps)
}

# return the set of files /subdirectories within a directory that should be
# crawled for dependencies
renv_dependencies_discover_dir_children <- function(path, root) {

  # for R packages, use a pre-defined set of files
  # (TODO: should we still respect .renvignore etc. here?)
  if (all(file.exists(file.path(path, c("DESCRIPTION", "NAMESPACE"))))) {
    children <- file.path(path, c("DESCRIPTION", "R", "tests", "vignettes"))
    return(children[file.exists(children)])
  }

  # list files in the folder
  children <- renv_file_list(path, full.names = TRUE)

  # remove files which are broken symlinks
  children <- children[file.exists(children)]

  # remove hard-coded ignores
  ignored <- c("renv")
  children <- children[!basename(children) %in% ignored]

  # construct pattern for matching files in this path
  # (return all files if no such pattern available)
  pattern <- renv_renvignore_pattern(path, root)
  if (empty(pattern))
    return(children)

  # we had a pattern; use it to match against file entries
  grep(pattern, children, perl = TRUE, invert = TRUE, value = TRUE)

}

renv_dependencies_discover_description <- function(path, fields = NULL) {

  dcf <- catch(renv_description_read(path))
  if (inherits(dcf, "error"))
    return(list())

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

    renv_dependencies_list(
      path,
      extract_chr(matches, 2L),
      extract_chr(matches, 3L),
      extract_chr(matches, 4L)
    )

  })

  # attempt to infer dependency on devtools -- note that this is only
  # relevant if we're analyizing the DESCRIPTION associating with the
  # current project
  if (renv_file_same(path, file.path(renv_project(), "DESCRIPTION"))) {
    if (any(c("Roxygen", "RoxygenNote", "Remotes") %in% names(dcf))) {
      devdeps <- renv_dependencies_list(path, c("roxygen2", "devtools"))
      data[[length(data) + 1]] <- devdeps
    }
  }

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
      return(NULL)

  deps <- stack()
  deps$push("rmarkdown")

  yaml <- rmarkdown::yaml_front_matter(path)

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
    return(NULL)

  # figure out the appropriate begin, end patterns
  type <- tolower(tools::file_ext(path))
  if (type %in% c("rmd", "rmarkdown"))
    type <- "md"

  patterns <- knitr::all_patterns[[type]]
  if (is.null(patterns)) {
    warningf("'%s' is not a recognized multi-mode R document.", path)
    return(character())
  }

  # parse the chunks within
  # NOTE: we need to proceed line-by-line since the chunk end pattern might
  # end chunks not started by the chunk begin pattern (sad face)
  encoding <- if (type == "md") "UTF-8" else "unknown"
  contents <- readLines(path, warn = FALSE, encoding = encoding)
  ranges <- renv_dependencies_discover_chunks_ranges(file, contents, patterns)

  # extract chunk code from the used ranges
  chunks <- .mapply(function(lhs, rhs) {
    header <- contents[[lhs]]
    params <- renv_dependencies_discover_parse_params(header, type)
    list(params = params, contents = contents[(lhs + 1):(rhs - 1)])
  }, ranges, NULL)

  # iterate over chunks, and attempt to parse dependencies from each
  cdeps <- bapply(chunks, function(chunk) {

    # skip non-R chunks
    engine <- chunk$params$engine
    if (!(identical(engine, "r") || identical(engine, "rscript")))
      return(character())

    deps <- catch(renv_dependencies_discover_r(file = path, text = chunk$contents))
    if (inherits(deps, "error"))
      return(NULL)

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
    return(NULL)

  text <- unlist(regmatches(pasted, matches), use.names = FALSE, recursive = FALSE)
  code <- substring(text, 4L, nchar(text) - 1L)
  renv_dependencies_discover_r(file = path, text = code)

}

renv_dependencies_discover_chunks_ranges <- function(file, contents, patterns) {

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

    if (chunk == TRUE && grepl(patterns$chunk.end, line)) {
      chunk <- FALSE
      end <- i
      output[[length(output) + 1]] <- list(lhs = start, rhs = end)
    }

  }

  if (chunk)
    warningf("[Line %i]: detected unclosed chunk in file '%s'", file, start)

  bind_list(output)

}

renv_dependencies_discover_rproj <- function(path) {

  props <- renv_read_properties(path)

  deps <- stack()
  if (identical(props$PackageUseDevtools, "Yes")) {
    deps$push("devtools")
    deps$push("roxygen2")
  }

  renv_dependencies_list(path, deps$data())

}

renv_dependencies_discover_r <- function(file = NULL, text = NULL) {

  parsed <- catch(renv_parse(file = file, text = text))
  if (inherits(parsed, "error")) {
    # workaround for an R bug where parse-related state could be
    # leaked if an error occurred
    Sys.setlocale()
    return(character())
  }

  methods <- c(
    renv_dependencies_discover_r_xfun,
    renv_dependencies_discover_r_library_require,
    renv_dependencies_discover_r_require_namespace,
    renv_dependencies_discover_r_colon
  )

  discoveries <- new.env(parent = emptyenv())
  recurse(parsed, function(node) {
    for (method in methods)
      method(node, discoveries)
  })

  packages <- ls(envir = discoveries)
  if (empty(packages))
    return(NULL)

  renv_dependencies_list(file, packages)

}

renv_dependencies_discover_r_xfun <- function(node, envir) {

  node <- renv_call_expect(node, c("pkg_attach", "pkg_attach2"))
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

  node <- renv_call_expect(node, c("library", "require"))
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

  node <- renv_call_expect(node, c("requireNamespace", "loadNamespace"))
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

renv_dependencies_list <- function(source, packages, require = "", version = "") {

  if (empty(packages))
    return(NULL)

  source <- source %||% rep.int(NA_character_, length(packages))

  data.frame(
    Source  = as.character(source),
    Package = as.character(packages),
    Require = require,
    Version = version,
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

  # try to guess where the label is
  label <- ""
  idx <- regexpr("(?:[ ,=]|$)", rest)
  if (idx != -1) {
    ch <- substring(rest, idx, idx)
    if (ch != '=') {
      label <- substring(rest, 1, idx - 1)
      rest <- sub("^,*\\s*", "", substring(rest, idx + 1))
    }
  }

  params <- catch(parse(text = sprintf("alist(%s)", rest))[[1]])

  if (inherits(params, "error"))
    return(list(engine = engine))

  if (is.null(params[["label"]]) && nzchar(label))
    params[["label"]] <- label

  if (is.null(params[["engine"]]))
    params[["engine"]] <- engine

  eval(params, envir = parent.frame())

}

# find recursive dependencies of a package. note that this routine
# doesn't farm out to CRAN; it relies on the package and its dependencies
# all being installed locally. returns a named vector mapping package names
# to the path where they were discovered, or NA if those packages are not
# installed
renv_dependencies <- function(project, packages, fields = NULL) {

  # TODO: build a dependency tree rather than just a flat set of packages?
  # TODO: dependency resolution? (can we depend on a different package for this)
  # TODO: recursive and non-recursive dependencies?
  visited <- new.env(parent = emptyenv())
  ignored <- c("renv", settings$ignored.packages(project = project))
  packages <- setdiff(packages, ignored)
  for (package in packages)
    renv_dependencies_enumerate(package, visited, fields)

  as.list(visited)

}

renv_dependencies_enumerate <- function(package, visited, fields = NULL) {

  # skip the 'R' package
  if (package == "R")
    return()

  # if we've already visited this package, bail
  if (exists(package, envir = visited, inherits = FALSE))
    return()

  # default to unknown path for visited packages
  assign(package, NA, envir = visited, inherits = FALSE)

  # find the package
  libpaths <- c(renv_libpaths_user(), .Library.site, .Library)
  location <- renv_package_find(package, libpaths = libpaths)
  if (!file.exists(location))
    return(location)

  # we know the path, so set it now
  assign(package, location, envir = visited, inherits = FALSE)

  # find its dependencies from the DESCRIPTION file
  deps <- renv_dependencies_discover_description(location, fields)
  subpackages <- deps$Package
  for (subpackage in subpackages)
    renv_dependencies_enumerate(subpackage, visited)

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
