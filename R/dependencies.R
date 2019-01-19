
#' Discover R Package Dependencies
#'
#' Discover \R packages used within files and directories.
#'
#' @param path The path to a (possibly multi-mode) \R file, or a directory
#'   containing such files.
#'
#' @export
discover_dependencies <- function(path = getwd()) {

  info <- file.info(path)
  if (is.na(info$isdir))
    stopf("File '%s' does not exist.", path)

  if (info$isdir)
    return(renv_dependencies_discover_dir(path))

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

renv_dependencies_discover_dir <- function(path) {

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # list files in the folder
  children <- list.files(path, full.names = TRUE)

  # filter children based on pattern
  pattern <- sprintf("(?:%s)$", paste(renv_renvignore_get(), collapse = "|"))
  matches <- grep(pattern, children, perl = TRUE, value = TRUE, invert = TRUE)

  # recurse for dependencies
  deps <- lapply(matches, discover_dependencies)

  bind_list(deps)

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
  if (identical(renv_state$project(), dirname(path)))
    fields <- c(fields, "Suggests")

  data <- lapply(fields, function(field) {

    contents <- dcf[[field]]
    if (!is.character(contents))
      return(list())

    x <- strsplit(dcf[[field]], "\\s*,\\s*")[[1]]
    m <- regexec(pattern, x)
    matches <- regmatches(x, m)

    data.frame(
      Package = extract_chr(matches, 2L),
      Require = extract_chr(matches, 3L),
      Version = extract_chr(matches, 4L),
      stringsAsFactors = FALSE
    )

  })

  bound <- bind_list(data)
  if (is.null(bound))
    return(NULL)

  cbind(Source = path, bound, stringsAsFactors = FALSE)

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
  if (!renv_dependencies_require("knitr", "multi-mode files"))
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
  if (NROW(ranges) == 0)
    return(NULL)

  chunks <- .mapply(function(lhs, rhs) {
    header <- contents[[lhs]]
    params <- renv_dependencies_discover_parse_params(header, type)
    list(params = params, contents = contents[(lhs + 1):(rhs - 1)])
  }, ranges, NULL)

  # extract R code
  code <- uapply(chunks, function(chunk) {

    # skip non-R chunks
    engine <- chunk$params$engine
    if (!(identical(engine, "r") || identical(engine, "rscript")))
      return(character())

    chunk$contents

  })

  # write to file and parse dependencies
  rfile <- tempfile(fileext = ".R")
  writeLines(enc2utf8(code), con = rfile, useBytes = TRUE)
  deps <- renv_dependencies_discover_r(rfile)
  if (empty(deps))
    return(NULL)

  deps$Source <- path
  deps

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

renv_dependencies_discover_r <- function(path) {

  parsed <- catch(renv_parse(path))
  if (inherits(parsed, "error")) {
    # workaround for an R bug where parse-related state could be
    # leaked if an error occurred
    Sys.setlocale()
    return(character())
  }

  methods <- c(
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

  renv_dependencies_list(path, packages)

}

renv_dependencies_discover_r_library_require <- function(node, envir) {

  ok <-
    is.call(node) &&
    is.name(node[[1]]) &&
    as.character(node[[1]]) %in% c("library", "require")

  if (!ok)
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

  ok <-
    is.call(node) &&
    is.name(node[[1]]) &&
    as.character(node[[1]]) %in% c("requireNamespace", "loadNamespace")

  if (!ok)
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

renv_dependencies_list <- function(source, packages) {

  if (empty(packages))
    return(NULL)

  data.frame(
    Source  = as.character(source),
    Package = as.character(packages),
    Require = "",
    Version = "",
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
renv_dependencies <- function(packages, fields = NULL) {

  # TODO: build a dependency tree rather than just a flat set of packages?
  # TODO: dependency resolution? (can we depend on a different package for this)
  # TODO: recursive and non-recursive dependencies?
  visited <- new.env(parent = emptyenv())
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
  location <- renv_package_find(package)
  if (!renv_file_exists(location))
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
