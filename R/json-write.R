
# @param box A vector of names, whose values should be boxed. By default,
#   scalar values are unboxed.
renv_json_config <- function(box = character()) {
  list(box = box)
}

renv_json_write <- function(object,
                            config = NULL,
                            file = stdout())
{
  config <- config %||% renv_json_config()
  json <- renv_json_convert_impl(NULL, object, config, 0L)
  if (is.null(file))
    return(json)

  writeLines(json, con = file)

}

renv_json_convert <- function(object, config = renv_json_config()) {
  renv_json_convert_impl(NULL, object, config, 0L)
}

renv_json_convert_impl <- function(key, value, config, depth) {

  if (is.list(value) || !is.null(names(value)))
    return(renv_json_convert_list(key, value, config, depth))

  json <- renv_json_convert_atom(key, value, config, depth)
  indent <- renv_json_convert_indent(depth)
  paste0(indent, json)

}

renv_json_convert_list <- function(key, value, config, depth) {
  indent <- renv_json_convert_indent(depth)
  if (empty(value)) {
    json <- if (is.null(names(value))) "[]" else "{}"
    paste0(indent, json)
  } else if (is.null(names(value))) {
    json <- enum_chr(value, renv_json_convert_impl, config = config, depth = depth + 1L)
    paste0(indent, "[", "\n", paste(json, collapse = ",\n"), "\n", indent, "]")
  } else {
    keys <- renv_json_quote(names(value))
    vals <- enum_chr(value, renv_json_convert_impl, config = config, depth = depth + 1L)
    idx  <- regexpr("[^[:space:]]", vals)
    json <- paste0(substring(vals, 1L, idx - 1L), keys, ": ", substring(vals, idx))
    paste0(indent, "{", "\n", paste(json, collapse = ",\n"), "\n", indent, "}")
  }
}

renv_json_convert_atom <- function(key, value, config, depth) {

  unbox <- is.null(key) || !key %in% config$box || inherits(value, "AsIs")
  if (is.null(value))
    return(if (unbox) "null" else "[]")

  n <- length(value)
  if (n == 0L)
    return("[]")

  if (is.character(value)) {
    value <- renv_json_quote(value)
    value[value %in% c("NA")] <- "null"
  }

  if (is.logical(value)) {
    value <- ifelse(value, "true", "false")
    value[is.na(value)] <- "null"
  }

  if (unbox && n == 1L)
    return(if (is.na(value)) "null" else paste0(value))

  indent <- renv_json_convert_indent(depth)
  json <- paste0(renv_json_convert_indent(depth + 1L), value)
  paste0("[", "\n", paste(json, collapse = ",\n"), "\n", indent, "]")

}

renv_json_convert_indent <- function(level) {
  paste(rep("  ", level), collapse = "")
}
