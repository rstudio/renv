
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

  renv_json_remap(json, map)

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
      return(NA)
  }

  # recurse
  if (is.recursive(json)) {
    for (i in seq_along(json))
      json[[i]] <- renv_json_remap(json[[i]], map)
  }

  json

}
