
renv_json_write <- function(object, file = stdout()) {

  json <- renv_json_convert(object)
  if (is.null(file))
    return(json)

  writeLines(json, con = file)

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
    keys <- renv_json_quote(names(object))
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
    object <- renv_json_quote(object)
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
