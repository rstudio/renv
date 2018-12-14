
#' Discover R Package Dependencies
#'
#' Discover \R packages used within files and directories.
#'
#' @param path The path to a (possibly multi-mode) \R file, or a directory
#'   containing such files.
#'
#' @export
discover_dependencies <- function(path) {

  info <- file.info(path)
  if (is.na(info$isdir))
    stopf("File '%s' does not exist.", path)

  if (info$isdir)
    return(renv_dependencies_discover_dir(path))

  name <- basename(path)
  ext <- tolower(tools::file_ext(path))

  case(

    # special cases for special filenames
    name == "DESCRIPTION" ~ renv_dependencies_discover_description(path),

    # generic extension-based lookup
    ext == "r"   ~ renv_dependencies_discover_r(path),
    ext == "rmd" ~ renv_dependencies_discover_multimode(path, "rmd"),
    ext == "rnw" ~ renv_dependencies_discover_multimode(path, "rnw")

  )

}

renv_dependencies_discover_dir <- function(path) {

  # TODO: make this user-configurable?
  # TODO: maximum recursion depth?
  exclude <- c("node_modules", "packrat", "revdep")

  # list files in the folder
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- list.files(path)

  # filter children based on pattern
  pattern <- sprintf("^(?:%s)$", paste(exclude, collapse = "|"))
  matches <- grep(pattern, files, perl = TRUE, value = TRUE, invert = TRUE)

  # construct new file paths
  children <- file.path(path, matches)

  # recurse for dependencies
  deps <- lapply(children, discover_dependencies)

  bind_list(deps)

}

renv_dependencies_discover_description <- function(path) {

  dcf <- tryCatch(read.dcf(path, all = TRUE), error = identity)
  if (inherits(dcf, "error"))
    return(list())

  # TODO: make this user-configurable
  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")
  pattern <- "([a-zA-Z0-9._]+)(?:\\s*\\(([><=]+)\\s*([0-9.-]+)\\))?"

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

  cbind(Source = path, bind_list(data), stringsAsFactors = FALSE)

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

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {

    msg <- lines(
      "The 'rmarkdown' package is required to parse dependencies within .Rmd files.",
      "Consider installing it with `install.packages(\"rmarkdown\")`."
    )

    if (renv_global_once("rmarkdown.yaml.header"))
      warning(msg, call. = FALSE)

    return(character())

  }

  deps <- c("rmarkdown")
  yaml <- rmarkdown::yaml_front_matter(path)
  runtime <- yaml$runtime %||% ""
  if (grepl("shiny", runtime, fixed = TRUE))
    deps <- c(deps, "shiny")

  renv_dependencies_list(path, deps)

}

renv_dependencies_discover_chunks <- function(path) {

  # ensure 'knitr' is installed / available
  if (!requireNamespace("knitr", quietly = TRUE)) {

    msg <- lines(
      "The 'knitr' package is required to parse dependencies within multi-mode files.",
      "Consider installing it with `install.packages(\"knitr\")`."
    )

    if (renv_global_once("knitr.chunks"))
      warning(msg, call. = FALSE)

    return(character())

  }

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
  encoding <- if (type == "md") "UTF-8" else "unknown"
  contents <- readLines(path, warn = FALSE, encoding = encoding)
  starts <- grep(patterns$chunk.begin, contents)
  ends <- grep(patterns$chunk.end, contents)
  if (length(starts) != length(ends)) {
    warningf("Failed to extract chunks from file '%s'.", path)
    return(character())
  }

  chunks <- .mapply(function(lhs, rhs) {
    header <- contents[[lhs]]
    params <- renv_dependencies_discover_parse_params(header, type)
    list(params = params, contents = contents[(lhs + 1):(rhs - 1)])
  }, list(starts, ends), NULL)

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
  if (is.null(deps))
    return(NULL)

  deps$Source <- path
  deps

}

renv_dependencies_discover_r <- function(path) {

  parsed <- tryCatch(parse(path, encoding = "UTF-8"), error = identity)
  if (inherits(parsed, "error")) {
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
  matched <- tryCatch(match.call(base::library, node), error = identity)
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

  matched <- tryCatch(match.call(base::requireNamespace, node), error = identity)
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

  data.frame(
    Source  = source,
    Package = packages,
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

  params <- tryCatch(
    parse(text = sprintf("alist(%s)", rest))[[1]],
    error = identity
  )

  if (inherits(params, "error"))
    return(list(engine = engine))

  if (is.null(params[["label"]]) && nzchar(label))
    params[["label"]] <- label

  if (is.null(params[["engine"]]))
    params[["engine"]] <- engine

  eval(params, envir = parent.frame())

}
