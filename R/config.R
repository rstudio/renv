
renv_config_create <- function(envir, formals) {

  args <- mget(ls(envir = envir), envir = envir)
  args <- args[names(formals)]

  defns <- renv_config_definitions()
  enumerate(args, function(key, val) {
    validate <- defns[[key]]$validate
    if (!validate(val)) {
      fmt <- "'%s' is not a valid setting for '%s'"
      stopf(fmt, deparse(val), key)
    }
  })

  args

}

renv_ved_name <- function() {
  list(
    validate = is_scalar_character,
    encode   = identity,
    decode   = identity,
    comment  = "The name of the virtual environment."
  )
}

renv_ved_r_version <- function() {
  list(
    validate = function(x) inherits(x, "numeric_version"),
    encode   = format,
    decode   = numeric_version,
    comment  = "The R version."
  )
}


renv_ved_r_repos <- function() {
  list(
    validate = is_named,
    encode   = renv_repos_encode,
    decode   = renv_repos_decode,
    comment  = "The R repositories."
  )
}

renv_ved_r_libs <- function() {
  list(
    validate = function(x) is.character(x),
    encode   = function(x) paste(x, collapse = ", "),
    decode   = function(x) strsplit(x, "\\s*,\\s*")[[1]],
    comment  = "The R libraries."
  )
}

renv_ved_r_libs_overlay <- function() {
  list(
    validate = is.logical,
    encode   = format,
    decode   = as.logical,
    comment  = "Overlay requested libraries over the default R libraries?"
  )
}

renv_config_definitions <- function() {
  list(
    name           = renv_ved_name(),
    r_version      = renv_ved_r_version(),
    r_repos        = renv_ved_r_repos(),
    r_libs         = renv_ved_r_libs(),
    r_libs_overlay = renv_ved_r_libs_overlay()
  )
}

renv_config_read <- function(path, text = NULL) {

  text <- text %||% readLines(path, warn = FALSE, encoding = "UTF-8")
  extracted <- grep("^\\s*\\w", text, perl = TRUE, value = TRUE)
  joined <- paste(extracted, collapse = "\n")
  lines <- strsplit(joined, "\n(?!\\s)", perl = TRUE)[[1]]

  idx <- regexpr(":", lines, fixed = TRUE)

  keys <- trimws(substring(lines, 1, idx - 1))
  vals <- trimws(substring(lines, idx + 2))
  names(vals) <- keys

  defns <- renv_config_definitions()
  config <- enumerate(vals, function(key, val) {
    defns[[key]]$decode(val)
  })

  config

}

renv_config_write <- function(config, path, comment = TRUE) {

  defns <- renv_config_definitions()
  contents <- enumerate(config, function(key, val) {
    header <- if (comment) paste("#", defns[[key]]$comment) else NULL
    encoded <- defns[[key]]$encode(val)
    entry <- paste(key, encoded, sep = ": ")
    if (comment) paste(header, entry, sep = "\n") else entry
  })

  contents <- paste(contents, collapse = if (comment) "\n\n" else "\n")
  if (is.null(path))
    return(contents)

  write_lines(enc2utf8(contents), con = path)

}
