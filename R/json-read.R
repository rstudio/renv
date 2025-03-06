
renv_json_read <- function(file = NULL, text = NULL) {

  jlerr <- NULL

  # if jsonlite is loaded, use that instead
  if ("jsonlite" %in% loadedNamespaces()) {

    json <- tryCatch(renv_json_read_jsonlite(file, text), error = identity)
    if (!inherits(json, "error"))
      return(json)

    jlerr <- json

  }

  # otherwise, fall back to the default JSON reader
  json <- tryCatch(renv_json_read_default(file, text), error = identity)
  if (!inherits(json, "error"))
    return(json)

  # report an error
  if (!is.null(jlerr))
    stop(jlerr)
  else
    stop(json)

}

renv_json_read_jsonlite <- function(file = NULL, text = NULL) {
  text <- paste(text %||% readLines(file, warn = FALSE), collapse = "\n")
  jsonlite::fromJSON(txt = text, simplifyVector = FALSE)
}

renv_json_read_patterns <- function() {

  list(

    # objects
    list("{", "\t\n\tobject(\t\n\t", TRUE),
    list("}", "\t\n\t)\t\n\t",       TRUE),

    # arrays
    list("[", "\t\n\tarray(\t\n\t", TRUE),
    list("]", "\n\t\n)\n\t\n",      TRUE),

    # maps
    list(":", "\t\n\t=\t\n\t", TRUE),

    # newlines
    list("\\u000a", "\n", FALSE)

  )

}

renv_json_read_envir <- function() {

  envir <- new.env(parent = emptyenv())

  envir[["+"]] <- `+`
  envir[["-"]] <- `-`

  envir[["object"]] <- function(...) {
    result <- list(...)
    names(result) <- as.character(names(result))
    result
  }

  envir[["array"]] <- list

  envir[["true"]]  <- TRUE
  envir[["false"]] <- FALSE
  envir[["null"]]  <- NULL

  envir

}

renv_json_read_remap <- function(object, patterns) {

  # repair names if necessary
  if (!is.null(names(object))) {

    nms <- names(object)
    for (pattern in patterns)
      nms <- gsub(pattern[[2L]], pattern[[1L]], nms, fixed = TRUE)
    names(object) <- nms

  }

  # repair strings if necessary
  if (is.character(object)) {
    for (pattern in patterns)
      object <- gsub(pattern[[2L]], pattern[[1L]], object, fixed = TRUE)
  }

  # recurse for other objects
  if (is.recursive(object))
    for (i in seq_along(object))
      object[i] <- list(renv_json_read_remap(object[[i]], patterns))

  # return remapped object
  object

}

renv_json_read_default <- function(file = NULL, text = NULL) {

  # read json text
  text <- paste(text %||% readLines(file, warn = FALSE), collapse = "\n")

  # convert into something the R parser will understand
  patterns <- renv_json_read_patterns()
  transformed <- text
  for (pattern in patterns)
    transformed <- gsub(pattern[[1L]], pattern[[2L]], transformed, fixed = TRUE)

  # parse it
  rfile <- tempfile("renv-json-", fileext = ".R")
  on.exit(unlink(rfile), add = TRUE)
  writeLines(transformed, con = rfile)
  json <- parse(rfile, keep.source = FALSE, srcfile = NULL)[[1L]]

  # evaluate in safe environment
  result <- eval(json, envir = renv_json_read_envir())

  # fix up strings if necessary -- do so only with reversible patterns
  patterns <- Filter(function(pattern) pattern[[3L]], patterns)
  renv_json_read_remap(result, patterns)

}

