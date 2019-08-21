
# a very hacky, yet 100% R, JSON parser
renv_json_read <- function(file = NULL, text = NULL) {

  text <- paste(text %||% read(file), collapse = "\n")

  # find strings in the JSON
  pattern <- '["](?:(?:\\\\.)|(?:[^"\\\\]))*?["]'
  locs <- gregexpr(pattern, text)[[1]]

  # if any are found, replace them with placeholders
  replaced <- text
  strings <- character()
  replacements <- character()

  if (!identical(c(locs), -1L)) {

    # get the string values + compute replacements
    starts <- locs
    ends <- locs + attr(locs, "match.length") - 1L
    strings <- substring(text, starts, ends)
    replacements <- sprintf('"\032%i\032"', seq_along(strings))

    # replace the strings
    mapply(function(string, replacement) {
      replaced <<- sub(string, replacement, replaced, fixed = TRUE)
    }, strings, replacements)

  }

  # transform the JSON into something the R parser understands
  transformed <- replaced
  transformed <- gsub("[[{]", "list(", transformed)
  transformed <- gsub("[]}]", ")", transformed)
  transformed <- gsub(":", "=", transformed, fixed = TRUE)
  json <- parse(text = paste(transformed, collapse = "\n"))[[1]]

  # construct map between source strings, replaced strings
  map <- as.character(parse(text = strings))
  names(map) <- as.character(parse(text = replacements))

  # remap strings in object
  remapped <- renv_json_remap(json, map)

  # evaluate
  eval(remapped, envir = baseenv())

}

renv_json_remap <- function(json, map) {

  # fix names
  nm <- names(json)
  if (!is.null(nm))
    names(json) <- map[names(json)]

  # fix values
  if (is.character(json))
    return(map[[json]])

  # handle true, false, null
  if (is.name(json)) {
    text <- as.character(json)
    if (text == "true")
      return(TRUE)
    else if (text == "false")
      return(FALSE)
    else if (text == "null")
      return(NULL)
  }

  # recurse
  if (is.recursive(json)) {
    for (i in seq_along(json)) {
      json[i] <- list(renv_json_remap(json[[i]], map))
    }
  }

  json

}

renv_json_convert <- function(object, level = 0, unbox = TRUE) {

  if (is.list(object) || !is.null(names(object)))
    return(renv_json_convert_list(object, level, unbox))

  json <- renv_json_convert_atom(object, level, unbox)
  indent <- renv_json_convert_indent(level)
  paste0(indent, json)

}

renv_json_convert_list <- function(object, level, unbox) {
  indent <- renv_json_convert_indent(level)
  if (empty(object)) {
    json <- if (is.null(names(object))) "[]" else "{}"
    paste0(indent, json)
  } else if (is.null(names(object))) {
    json <- map_chr(object, renv_json_convert, level = level + 1, unbox = unbox)
    paste0(indent, "[", "\n", paste(json, collapse = ",\n"), "\n", indent, "]")
  } else {
    keys <- shQuote(names(object), type = "cmd")
    vals <- map_chr(object, renv_json_convert, level = level + 1, unbox = unbox)
    idx  <- regexpr("[^[:space:]]", vals)
    json <- paste0(substring(vals, 1, idx - 1L), keys, ": ", substring(vals, idx))
    paste0(indent, "{", "\n", paste(json, collapse = ",\n"), "\n", indent, "}")
  }
}

renv_json_convert_atom <- function(object, level, unbox) {

  if (is.null(object))
    return(if (unbox) "null" else "[]")

  n <- length(object)
  if (n == 0)
    return("[]")

  unbox <- unbox || inherits(object, "AsIs")

  if (is.character(object)) {
    object <- shQuote(encodeString(object), type = "cmd")
    object[object == "\"NA\""] <- "null"
  }

  if (is.logical(object)) {
    object <- ifelse(object, "true", "false")
    object[is.na(object)] <- "null"
  }

  if (unbox && n == 1)
    return(paste0(object))

  json <- paste0(object, collapse = ",")
  if (nchar(json) <= 80)
    return(paste0("[", json, "]"))

  indent <- renv_json_convert_indent(level)
  json <- paste0(renv_json_convert_indent(level + 1), object)
  paste0("[", "\n", paste(json, collapse = ",\n"), "\n", indent, "]")

}

renv_json_convert_indent <- function(level) {
  paste(rep("  ", level), collapse = "")
}

renv_json_write <- function(object, file = stdout()) {

  json <- renv_json_convert(object)
  if (is.null(file))
    return(json)

  writeLines(json, con = file)

}
