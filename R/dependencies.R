
# TODO: allow for blacklist / whitelist of files when searching dependencies
# in a directory? where should this be set (project option? sensible default?)
# e.g. control whether we recurse into hidden folders?

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
    ext == "rmd" ~ renv_dependencies_discover_rmd(path)

  )

}

renv_dependencies_discover_dir <- function(path) {

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  children <- list.files(path, full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
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

renv_dependencies_discover_rmd <- function(path) {

  # TODO: dependencies from YAML
  # TODO: extract R code (skip non-evaled chunks?)
  # TODO: find in-line R code?

  deps <- c(
    renv_dependencies_discover_rmd_yaml_header(path),
    renv_dependencies_discover_chunks(path)
  )

  bind_list(Filter(NROW, deps))

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

  yaml <- rmarkdown::yaml_front_matter(path)
  runtime <- yaml$runtime %||% ""
  if (grepl("shiny", runtime, fixed = TRUE))
    return("shiny")

  character()

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
    params <- renv_dependencies_discover_parse_params(header, type, patterns)
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
  renv_dependencies_discover_r(rfile)

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
    Source  = path,
    Package = packages,
    Require = "",
    Version = "",
    stringsAsFactors = FALSE
  )

}

renv_dependencies_discover_parse_params <- function(header, type, patterns) {

  engine <- "r"
  rest <- sub(patterns$chunk.begin, "\\1", header)

  # if this is an R Markdown document, parse the initial engine chunk
  if (type == "md") {
    idx <- regexpr("[ ,]", rest)
    engine <- substring(rest, 1, idx - 1)
    rest <- substring(rest, idx + 1)
  }

  params <- tryCatch(
    parse(text = sprintf("list(%s)", rest))[[1]],
    error = identity
  )

  if (inherits(params, "error"))
    return(list(engine = engine))

  # if the first entry is not named, it's the label
  if (is.null(names(params)) || names(params)[[2]] == "") {
    idx <- regexpr("[ ,]", rest)
    params[["label"]] <- substring(rest, 1, idx - 1)
    params[[2L]] <- NULL
  }

  if (is.null(params[["engine"]]))
    params[["engine"]] <- engine

  params

}
